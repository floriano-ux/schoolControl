module Web.View.Courses.New where
import Web.View.Prelude

data NewView = NewView { course :: Course }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Novo curso</h1>
        {renderForm course}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Cursos" CoursesAction
                , breadcrumbText "Novo curso"
                ]

renderForm :: Course -> Html
renderForm course = formFor course [hsx|
    {(textField #name) { fieldLabel = "Nome" }}
    {(numberField #workload) { fieldLabel = "Carga horária (horas)" }}
    {submitButton { label = "Salvar" }}
|]
