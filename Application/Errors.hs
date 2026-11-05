-- | Tratamento de erros monádico da aplicação (Sprint 5).
--
-- Ideia central: toda falha que o usuário pode provocar ou que a infraestrutura
-- pode gerar vira um valor do tipo 'AppError', e os fluxos que encadeiam etapas
-- rodam no monad 'AppM' (= 'ExceptT AppError IO'). Assim:
--
--   * a primeira etapa que falha interrompe o fluxo (curto-circuito do ExceptT),
--     sem 'case' aninhado em cada passo;
--   * exceções de IO (banco fora do ar, violação de UNIQUE/FK/CHECK) são
--     capturadas UMA vez em 'capturarInfra' e traduzidas para um 'AppError';
--   * a decisão de "como responder" (re-renderizar formulário, redirecionar,
--     status HTTP) fica no controller, que só lida com 'Either AppError a'.
--
-- As funções de tradução ('classificarFalhaSql', 'mensagemAppError',
-- 'codigoHttp') são puras e cobertas por testes unitários.
module Application.Errors
    ( AppError (..)
    , AppM
    , executarApp
    , falharCom
    , exigirJust
    , deEither
    , capturarInfra
    , classificarFalhaSql
    , mensagemAppError
    , codigoHttp
    , deErroMatricula
    , deErroSenha
    , deErrosValidacao
    ) where

import IHP.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Control.Exception (try, SomeException, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, except)
import System.IO (stderr)

import Application.Domain (ErroMatricula, mensagemErroMatricula, ErroValidacao, mensagemDeErro)
import Application.Security (ErroSenha, mensagemDaSenha)


-- | Todas as falhas conhecidas da aplicação, num único tipo soma.
data AppError
    = ErroDeValidacao [Text]  -- ^ dados de entrada inválidos (uma mensagem por problema)
    | NaoEncontrado Text      -- ^ registro inexistente (o texto diz qual)
    | Conflito Text           -- ^ violação de unicidade / estado conflitante
    | NaoAutenticado          -- ^ precisa fazer login
    | SemPermissao            -- ^ logado, mas o papel não permite a operação
    | FalhaDeInfra Text       -- ^ erro técnico; o detalhe vai só para o log
    deriving (Eq, Show)

-- | Monad dos fluxos de negócio com efeitos: IO + curto-circuito em 'AppError'.
type AppM = ExceptT AppError IO

-- | Executa um fluxo 'AppM' e devolve o resultado como 'Either'.
executarApp :: AppM a -> IO (Either AppError a)
executarApp = runExceptT

-- | Interrompe o fluxo com um erro.
falharCom :: AppError -> AppM a
falharCom = throwE

-- | Converte um 'Maybe' em sucesso ou no erro informado (ex.: registro não achado).
exigirJust :: AppError -> Maybe a -> AppM a
exigirJust erro = maybe (throwE erro) pure

-- | Eleva uma validação pura ('Either') para o fluxo, traduzindo o erro.
deEither :: (e -> AppError) -> Either e a -> AppM a
deEither traduzir = except . either (Left . traduzir) Right

-- | Executa uma ação de IO (tipicamente banco de dados) capturando QUALQUER
-- exceção. O detalhe técnico é registrado no log do servidor e a falha é
-- classificada em um 'AppError' (conflito, validação ou infraestrutura).
capturarInfra :: Text -> IO a -> AppM a
capturarInfra contexto acao = do
    resultado <- liftIO (try acao)
    case resultado of
        Right valor -> pure valor
        Left (excecao :: SomeException) -> do
            let detalhe = cs (displayException excecao) :: Text
            liftIO (Text.IO.hPutStrLn stderr ("[ERROR] [" <> contexto <> "] " <> detalhe))
            throwE (classificarFalhaSql detalhe)

-- | Classifica a mensagem de uma exceção do PostgreSQL pelo SQLSTATE.
-- 23505 = unique_violation, 23503 = foreign_key_violation, 23514 = check_violation.
classificarFalhaSql :: Text -> AppError
classificarFalhaSql detalhe
    | contem ["23505", "duplicate key"]        = Conflito "Já existe um registro com esses dados."
    | contem ["23503", "foreign key"]          = ErroDeValidacao ["Referência inválida: o registro relacionado não existe."]
    | contem ["23514", "violates check"]       = ErroDeValidacao ["Valor fora do intervalo permitido."]
    | otherwise                                = FalhaDeInfra detalhe
  where
    contem = any (`Text.isInfixOf` Text.toLower detalhe) . map Text.toLower

-- | Mensagem amigável para o usuário final. Nunca expõe detalhes técnicos.
mensagemAppError :: AppError -> Text
mensagemAppError erro = case erro of
    ErroDeValidacao []        -> "Dados inválidos."
    ErroDeValidacao mensagens -> Text.intercalate " " mensagens
    NaoEncontrado oQue        -> oQue <> " não encontrado(a)."
    Conflito mensagem         -> mensagem
    NaoAutenticado            -> "Faça login para continuar."
    SemPermissao              -> "Você não tem permissão para esta operação."
    FalhaDeInfra _            -> "Estamos com um problema técnico no momento. Tente novamente em instantes."

-- | Código HTTP correspondente (útil para respostas JSON/API e para documentação).
codigoHttp :: AppError -> Int
codigoHttp erro = case erro of
    ErroDeValidacao _ -> 422
    NaoEncontrado _   -> 404
    Conflito _        -> 409
    NaoAutenticado    -> 401
    SemPermissao      -> 403
    FalhaDeInfra _    -> 503

-- Tradutores dos erros específicos de cada módulo para o erro geral.

deErroMatricula :: ErroMatricula -> AppError
deErroMatricula = ErroDeValidacao . pure . mensagemErroMatricula

deErroSenha :: ErroSenha -> AppError
deErroSenha = ErroDeValidacao . pure . mensagemDaSenha

deErrosValidacao :: [ErroValidacao] -> AppError
deErrosValidacao = ErroDeValidacao . map mensagemDeErro
