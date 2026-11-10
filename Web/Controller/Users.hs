module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Index
import Web.View.Users.New
import Web.View.Users.Show
import Web.View.Users.Edit
import qualified Data.Text as Text

-- Cadastro de usuários e papéis: somente Administradores.
instance Controller UsersController where
    beforeAction = exigirPermissao Gerenciar Usuarios

    action UsersAction = do
        users <- query @User |> orderBy #name |> fetch
        render IndexView { .. }

    action ShowUserAction { userId } = do
        user <- fetch userId
        config <- carregarAuthConfig
        let documento = decifrarOpcional config user.document
        disciplinas <- query @Subject |> filterWhere (#professorId, Just userId) |> orderBy #name |> fetch
        render ShowView { user, documento, disciplinas }

    action NewUserAction = do
        let user = newRecord @User
        render NewView { .. }

    action CreateUserAction = do
        config <- carregarAuthConfig
        let user = newRecord @User
        user
            |> buildUser
            |> validateField #passwordHash (viaSenha config)
            |> validarEmailDeUsuarioUnico
            >>= ifValid \case
                Left user -> render NewView { user = user |> set #passwordHash "" }
                Right user -> do
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
        config <- carregarAuthConfig
        user <- fetch userId
        let hashAtual  = user.passwordHash
        let docAtual   = user.document
        let papelAtual = user.userRole
        user
            |> buildUser
            |> validateField #passwordHash (senhaOpcional config)
            |> validarEmailDeUsuarioUnico
            >>= validarTrocaDePapel papelAtual
            >>= ifValid \case
                Left user -> render EditView { user = user |> set #passwordHash "", documento = "" }
                Right user -> do
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

senhaOpcional :: AuthConfig -> Text -> ValidatorResult
senhaOpcional config senha
    | Text.null senha = Success
    | otherwise       = viaSenha config senha

validarEmailDeUsuarioUnico user = user
    |> withCustomErrorMessageIO "Este e-mail já está em uso" validateIsUnique #email

validarTrocaDePapel papelAtual user = do
    vinculos <- case papelAtual of
        Aluno     -> query @Enrollment |> filterWhere (#studentId, user.id) |> fetchCount
        Professor -> query @Subject |> filterWhere (#professorId, Just user.id) |> fetchCount
        _         -> pure 0
    pure if podeTrocarPapel papelAtual user.userRole vinculos
        then user
        else user |> attachFailure #userRole (mensagemTrocaDePapel papelAtual)
