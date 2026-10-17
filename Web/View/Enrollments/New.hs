module Web.View.Enrollments.New where
import Web.View.Prelude

data NewView = NewView
    { enrollment :: Enrollment
    , students   :: [Student]
    , courses    :: [Course]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Nova matrícula</h1>
        {renderForm enrollment students courses}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                , breadcrumbText "Nova matrícula"
                ]

renderForm :: Enrollment -> [Student] -> [Course] -> Html
renderForm enrollment students courses = formFor enrollment [hsx|
    {(selectField #studentId students) { fieldLabel = "Aluno", placeholder = "Selecione o aluno" }}
    {(selectField #courseId courses) { fieldLabel = "Curso", placeholder = "Selecione o curso" }}
    {(selectField #status statuses) { fieldLabel = "Status" }}
    {submitButton { label = "Matricular" }}
|]
    where
        statuses = [minBound .. maxBound] :: [EnrollmentStatus]
