module Web.View.Students.New where
import Web.View.Prelude

data NewView = NewView { student :: User }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Novo estudante</h1>
        <p class="text-muted">Uma senha aleatória será gerada e exibida uma única vez após o cadastro.</p>
        {renderForm student}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Estudantes" StudentsAction
                , breadcrumbText "Novo estudante"
                ]

renderForm :: User -> Html
renderForm student = formFor' student (pathTo CreateStudentAction) [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(emailField #email) { fieldLabel = "E-mail" }}
    {submitButton { label = "Salvar" }}
|]
