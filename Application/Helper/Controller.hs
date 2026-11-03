module Application.Helper.Controller
    ( viaDominio
    , exigirPermissao
    , opcoesDeMatricula
    , tentar
    , logAuthInfo
    , logAuthWarn
    ) where

import IHP.ControllerPrelude
import Generated.Types
import Web.Types
import Web.Routes ()
import Application.Domain
import Control.Exception (try, SomeException, displayException)
import qualified Data.Text.IO as Text.IO
import System.IO (stderr)

-- Here you can add functions which are available in all your controllers

-- Adapta uma validação pura do domínio (Either) para um validador do IHP.
-- Ex.: validateField #workload (viaDominio validarCargaHoraria)
viaDominio :: (valor -> Either ErroValidacao valor) -> valor -> ValidatorResult
viaDominio validar valor = case validar valor of
    Right _   -> Success
    Left erro -> Failure (mensagemDeErro erro)

-- Garante que há um usuário logado e que o papel dele permite a operação.
-- Sem login: redireciona para a tela de login (e volta para cá depois de entrar).
-- Sem permissão: volta para a página inicial com uma mensagem de erro.
-- No IHP 1.6 as actions devolvem IO ResponseReceived; para "sair no meio" de uma
-- action usamos earlyReturn, como faz o próprio ensureIsUser.
exigirPermissao operacao recurso = case currentUserOrNothing @User of
    Nothing -> do
        setErrorMessage "Faça login para acessar esta página."
        setSession "IHP.LoginSupport.redirectAfterLogin" getRequestPathAndQuery
        earlyReturn (redirectTo NewSessionAction)
    Just usuario ->
        unless (podeAcessar usuario.userRole operacao recurso) do
            setErrorMessage "Você não tem permissão para acessar esta página."
            earlyReturn (redirectToPath "/")

-- ---------------------------------------------------------------------------
-- Tratamento de erros e logs dos fluxos de autenticação
-- ---------------------------------------------------------------------------

-- Executa uma computação de IO (tipicamente acesso ao banco) capturando falhas
-- inesperadas — por exemplo, banco indisponível ou fora de sincronia com o
-- schema. O detalhe técnico vai para o log do servidor (back) e devolve-se uma
-- mensagem amigável em 'Left'. Como NÃO envolve render/redirect, o fluxo normal
-- de resposta do IHP nunca é interceptado: o controller decide como responder.
tentar :: Text -> IO a -> IO (Either Text a)
tentar fluxo acao = do
    resultado <- try acao
    case resultado of
        Right valor -> pure (Right valor)
        Left (erro :: SomeException) -> do
            escreverLog "ERROR" ("[" <> fluxo <> "] falha inesperada: " <> cs (displayException erro))
            pure (Left "Estamos com um problema técnico no momento. Tente novamente em instantes.")

-- Log informativo de um evento de autenticação (ex.: login bem-sucedido).
logAuthInfo :: Text -> IO ()
logAuthInfo mensagem = escreverLog "INFO" ("[auth] " <> mensagem)

-- Log de alerta para tentativas malsucedidas (credenciais erradas, conta bloqueada).
logAuthWarn :: Text -> IO ()
logAuthWarn mensagem = escreverLog "WARN" ("[auth] " <> mensagem)

-- Escreve uma linha no log do servidor (stderr), que o IHP exibe no terminal.
escreverLog :: Text -> Text -> IO ()
escreverLog nivel mensagem = Text.IO.hPutStrLn stderr ("[" <> nivel <> "] " <> mensagem)

-- Opções do select de matrícula, em ordem alfabética: "Aluno — Curso (status)".
opcoesDeMatricula :: [Student] -> [Course] -> [Enrollment] -> [OpcaoMatricula]
opcoesDeMatricula students courses enrollments =
    sortOn (.rotulo) (map opcao enrollments)
  where
    opcao enrollment = OpcaoMatricula
        { matriculaId = enrollment.id
        , rotulo = nomeDoAluno students enrollment.studentId
            <> " — " <> nomeDoCurso courses enrollment.courseId
            <> " (" <> nomeDoStatus enrollment.status <> ")"
        }
