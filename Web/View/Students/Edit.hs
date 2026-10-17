module Web.View.Students.Edit where
import Web.View.Prelude

data EditView = EditView { student :: Student }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar estudante</h1>
        {renderForm student}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Estudantes" StudentsAction
                , breadcrumbText "Editar estudante"
                ]

renderForm :: Student -> Html
renderForm student = formFor student [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {submitButton { label = "Salvar alterações" }}
|]
