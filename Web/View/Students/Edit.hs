module Web.View.Students.Edit where
import Web.View.Prelude

data EditView = EditView { student :: User }

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

renderForm :: User -> Html
renderForm student = formFor' student (pathTo (UpdateStudentAction student.id)) [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {submitButton { label = "Salvar alterações" }}
|]
