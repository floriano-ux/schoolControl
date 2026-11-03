module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Index
import Web.View.Users.New
import Web.View.Users.Edit
import qualified Data.Text as Text

-- Cadastro de usuários e papéis: somente Administradores.
instance Controller UsersController where
    beforeAction = exigirPermissao Gerenciar Usuarios

    action UsersAction = do
        users <- query @User |> orderBy #name |> fetch
        render IndexView { .. }

    action NewUserAction = do
        let user = newRecord @User
        render NewView { .. }

    action CreateUserAction = do
        let user = newRecord @User
        user
            |> buildUser
            |> validateField #passwordHash (hasMinLength 6 |> withCustomErrorMessage "A senha precisa ter pelo menos 6 caracteres")
            |> validarEmailDeUsuarioUnico
            >>= ifValid \case
                Left user -> render NewView { user = user |> set #passwordHash "" }
                Right user -> do
                    config <- carregarAuthConfig
                    resultado <- tentar "usuarios" do
                        hashed <- gerarHashSenha config user.passwordHash
                        documento <- cifrarOpcional config (param @Text "document")
                        user
                            |> set #passwordHash hashed
                            |> set #document documento
                            |> createRecord
                    case resultado of
                        Left mensagem -> do
                            setErrorMessage mensagem
                            render NewView { user = user |> set #passwordHash "" }
                        Right _ -> do
                            logAuthInfo ("usuário criado pelo admin: " <> user.email)
                            setSuccessMessage "Usuário criado"
                            redirectTo UsersAction

    action EditUserAction { userId } = do
        user <- fetch userId
        config <- carregarAuthConfig
        -- Nunca manda o hash da senha para o navegador; o CPF vai decifrado para o form.
        render EditView
            { user = user |> set #passwordHash ""
            , documento = fromMaybe "" (decifrarOpcional config user.document)
            }

    action UpdateUserAction { userId } = do
        user <- fetch userId
        let hashAtual = user.passwordHash
        let docAtual  = user.document
        user
            |> buildUser
            |> validateField #passwordHash senhaOpcional
            |> validarEmailDeUsuarioUnico
            >>= ifValid \case
                Left user -> render EditView { user = user |> set #passwordHash "", documento = "" }
                Right user -> do
                    config <- carregarAuthConfig
                    resultado <- tentar "usuarios" do
                        novoHash <- if Text.null user.passwordHash
                            then pure hashAtual
                            else gerarHashSenha config user.passwordHash
                        let docParam = param @Text "document"
                        documento <- if Text.null (Text.strip docParam)
                            then pure docAtual
                            else cifrarOpcional config docParam
                        user
                            |> set #passwordHash novoHash
                            |> set #document documento
                            |> updateRecord
                    case resultado of
                        Left mensagem -> do
                            setErrorMessage mensagem
                            render EditView { user = user |> set #passwordHash "", documento = "" }
                        Right _ -> do
                            logAuthInfo ("usuário atualizado pelo admin: " <> user.email)
                            setSuccessMessage "Usuário atualizado"
                            redirectTo UsersAction

    action DeleteUserAction { userId } = do
        user <- fetch userId
        if user.id == currentUserId
            then setErrorMessage "Você não pode excluir a sua própria conta."
            else do
                deleteRecord user
                setSuccessMessage "Usuário excluído"
        redirectTo UsersAction

buildUser user = user
    |> fill @'["name", "email", "userRole", "passwordHash"]
    |> validateField #name (nonEmpty |> withCustomErrorMessage "Informe o nome")
    |> validateField #email (isEmail |> withCustomErrorMessage "Informe um e-mail válido")

senhaOpcional :: Text -> ValidatorResult
senhaOpcional senha
    | Text.null senha = Success
    | otherwise       = senha |> (hasMinLength 6 |> withCustomErrorMessage "A senha precisa ter pelo menos 6 caracteres")

validarEmailDeUsuarioUnico user = user
    |> withCustomErrorMessageIO "Este e-mail já está em uso" validateIsUnique #email
