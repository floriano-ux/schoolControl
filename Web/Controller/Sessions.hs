module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import qualified IHP.AuthSupport.Lockable as Lockable

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User

    -- Mesmo fluxo do Sessions.createSessionAction do IHP (bloqueio após 10 tentativas,
    -- volta para a página que o usuário tentou abrir), mas com mensagens em português,
    -- logs no servidor e tratamento amigável de falhas técnicas (ex.: banco fora do ar).
    action CreateSessionAction = do
        let email = param @Text "email"
        let credenciaisInvalidas = do
                logAuthWarn ("credenciais inválidas no login de " <> email)
                setErrorMessage "E-mail ou senha incorretos."
                redirectTo NewSessionAction
        let falhaTecnica mensagem = do
                setErrorMessage mensagem
                redirectTo NewSessionAction
        -- A busca no banco é o ponto que pode estourar por problemas de infraestrutura.
        usuario <- tentar "login" do
            query @User
                |> filterWhereCaseInsensitive (#email, email)
                |> fetchOneOrNothing
        case usuario of
            Left mensagem    -> falhaTecnica mensagem
            Right Nothing    -> credenciaisInvalidas
            Right (Just user) -> do
                bloqueado <- Lockable.isLocked user
                if bloqueado
                    then do
                        logAuthWarn ("login recusado: conta bloqueada (" <> email <> ")")
                        setErrorMessage "Conta bloqueada por excesso de tentativas. Tente novamente em 1 hora."
                        redirectTo NewSessionAction
                    else if conferirSenha (param @Text "password") user.passwordHash
                        then do
                            login user
                            user
                                |> set #failedLoginAttempts 0
                                |> updateRecord
                            logAuthInfo ("login bem-sucedido: " <> email)
                            destino <- getSessionAndClear "IHP.LoginSupport.redirectAfterLogin"
                            redirectToPathSeeOther (fromMaybe "/" destino)
                        else do
                            user <- user
                                |> incrementField #failedLoginAttempts
                                |> updateRecord
                            when (user.failedLoginAttempts >= Sessions.maxFailedLoginAttempts user) do
                                Lockable.lock user
                                pure ()
                            credenciaisInvalidas

    action DeleteSessionAction = Sessions.deleteSessionAction @User

instance Sessions.SessionsControllerConfig User
