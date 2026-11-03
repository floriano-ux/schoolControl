module Web.View.Users.Edit where
import Web.View.Prelude

data EditView = EditView
    { user      :: User
    , documento :: Text 
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar usuário</h1>
        {renderForm user documento}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                , breadcrumbText "Editar usuário"
                ]

renderForm :: User -> Text -> Html
renderForm user documento = formFor user [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {(selectField #userRole papeis) { fieldLabel = "Papel" }}
    {(passwordField #passwordHash) { fieldLabel = "Nova senha", helpText = "Deixe em branco para manter a senha atual" }}
    <div class="form-group mb-3">
        <label class="form-label" for="document">CPF</label>
        <input id="document" name="document" type="text" class="form-control" value={documento} placeholder="Opcional — guardado cifrado" />
    </div>
    {submitButton { label = "Salvar alterações" }}
|]
    where
        papeis = [minBound .. maxBound] :: [UserRole]
