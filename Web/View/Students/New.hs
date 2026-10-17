module Web.View.Students.New where
import Web.View.Prelude

data NewView = NewView { student :: Student }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Novo estudante</h1>
        {renderForm student}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Estudantes" StudentsAction
                , breadcrumbText "Novo estudante"
                ]

renderForm :: Student -> Html
renderForm student = formFor student [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {submitButton { label = "Salvar" }}
|]
