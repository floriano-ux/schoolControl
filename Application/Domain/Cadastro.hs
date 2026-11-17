module Application.Domain.Cadastro
    ( DadosCadastro (..)
    , ErroCadastro (..)
    , validarCadastro
    , mensagemDoCadastro
    , pipelineCadastroCPS
    , variacoesDeEmail
    , primeiroEmailLivre
    ) where

import IHP.Prelude
import Application.Domain.Validacao (emailValido)
import qualified Data.Text as Text

-- Dados brutos vindos do formulário de cadastro.
data DadosCadastro = DadosCadastro
    { nomeCadastro        :: Text
    , emailCadastro       :: Text
    , senhaCadastro       :: Text
    , confirmacaoCadastro :: Text
    } deriving (Eq, Show)

data ErroCadastro
    = NomeObrigatorio
    | EmailInvalidoCadastro
    | SenhasNaoConferem
    deriving (Eq, Show)

-- Devolve Right com os dados quando tudo passa, ou Left com a lista de erros.
validarCadastro :: DadosCadastro -> Either [ErroCadastro] DadosCadastro
validarCadastro dados = case errosAtivos of
    []   -> Right dados
    erros -> Left erros
  where
    -- Coleção de regras: (condição que indica erro, erro correspondente).
    regras :: [(Bool, ErroCadastro)]
    regras =
        [ (Text.null (Text.strip dados.nomeCadastro),          NomeObrigatorio)
        , (not (emailValido dados.emailCadastro),              EmailInvalidoCadastro)
        , (dados.senhaCadastro /= dados.confirmacaoCadastro,   SenhasNaoConferem)
        ]
    -- filter + map (fmap de listas) sobre a coleção de regras.
    errosAtivos = map snd (filter fst regras)

mensagemDoCadastro :: ErroCadastro -> Text
mensagemDoCadastro erro = case erro of
    NomeObrigatorio       -> "Informe o seu nome"
    EmailInvalidoCadastro -> "Informe um e-mail válido"
    SenhasNaoConferem     -> "A senha e a confirmação não conferem"


-- ---------------------------------------------------------------------------
-- Pipeline de cadastro em Continuation Passing Style (CPS)
--
-- Em vez de devolver um Either e deixar o chamador casar o resultado, cada
-- etapa recebe explicitamente as continuações: 'onErro' (abortando com uma
-- mensagem) e 'onSucesso' (seguindo adiante). O fluxo fica linear e cada
-- verificação "continua" para a próxima apenas quando passa. As partes
-- efetivas (força da senha, e-mail já usado) são avaliadas fora e entram
-- prontas, mantendo esta função pura e testável.
-- ---------------------------------------------------------------------------

pipelineCadastroCPS
    :: DadosCadastro              -- dados do formulário
    -> Either Text ()             -- força da senha já avaliada (Left = mensagem de erro)
    -> Maybe Text                 -- e-mail em uso? Nothing = livre; Just = mensagem (com sugestão)
    -> (Text -> r)                -- continuação de erro
    -> (DadosCadastro -> r)       -- continuação de sucesso
    -> r
pipelineCadastroCPS dados forcaSenha emailEmUso onErro onSucesso =
    case validarCadastro dados of
        Left erros -> onErro (mensagemDoPrimeiro erros)
        Right _    -> case forcaSenha of
            Left mensagem -> onErro mensagem
            Right ()      -> case emailEmUso of
                Just mensagem -> onErro mensagem
                Nothing       -> onSucesso dados
  where
    mensagemDoPrimeiro erros = case erros of
        (erro : _) -> mensagemDoCadastro erro
        []         -> "Dados inválidos"

-- ---------------------------------------------------------------------------
-- Avaliação preguiçosa (lazy evaluation)
-- ---------------------------------------------------------------------------

variacoesDeEmail :: Text -> [Text]
variacoesDeEmail email = email : [ comSufixo n | n <- [1 ..] ]
  where
    (local, dominio) = Text.breakOn "@" email 
    comSufixo n = local <> tshow (n :: Int) <> dominio

primeiroEmailLivre :: [Text] -> Text -> Text
primeiroEmailLivre usados email =
    case filter (`notElem` usados) (variacoesDeEmail email) of
        (livre : _) -> livre
        []          -> email  -- inatingível: a lista é infinita