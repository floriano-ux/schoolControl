module Web.View.Users.New where
import Web.View.Prelude

data NewView = NewView { user :: User }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Novo usuário</h1>
        {renderForm user}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                , breadcrumbText "Novo usuário"
                ]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {(selectField #userRole papeis) { fieldLabel = "Papel" }}
    {(passwordField #passwordHash) { fieldLabel = "Senha", helpText = "Mínimo de 8 caracteres, com pelo menos um número e uma letra maiúscula" }}
    <div class="form-group mb-3">
        <label class="form-label" for="document">CPF</label>
        <input id="document" name="document" type="text" class="form-control" placeholder="Opcional — guardado cifrado" />
    </div>
    {submitButton { label = "Criar usuário" }}
|]
    where
        papeis = [minBound .. maxBound] :: [UserRole]
