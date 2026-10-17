module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import qualified IHP.AuthSupport.Lockable as Lockable

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User

    -- Mesmo fluxo do Sessions.createSessionAction do IHP (bloqueio após 10 tentativas,
    -- volta para a página que o usuário tentou abrir), mas com mensagens em português.
    action CreateSessionAction = do
        let credenciaisInvalidas = do
                setErrorMessage "E-mail ou senha incorretos."
                redirectTo NewSessionAction
        query @User
            |> filterWhereCaseInsensitive (#email, param "email")
            |> fetchOneOrNothing
            >>= \case
                Nothing -> credenciaisInvalidas
                Just user -> do
                    bloqueado <- Lockable.isLocked user
                    if bloqueado
                        then do
                            setErrorMessage "Conta bloqueada por excesso de tentativas. Tente novamente em 1 hora."
                            redirectTo NewSessionAction
                        else if verifyPassword user (param @Text "password")
                            then do
                                login user
                                user
                                    |> set #failedLoginAttempts 0
                                    |> updateRecord
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
