module Web.View.Users.Edit where
import Web.View.Prelude

data EditView = EditView { user :: User }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar usuário</h1>
        {renderForm user}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                , breadcrumbText "Editar usuário"
                ]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {(selectField #userRole papeis) { fieldLabel = "Papel" }}
    {(passwordField #passwordHash) { fieldLabel = "Nova senha", helpText = "Deixe em branco para manter a senha atual" }}
    {submitButton { label = "Salvar alterações" }}
|]
    where
        papeis = [minBound .. maxBound] :: [UserRole]
