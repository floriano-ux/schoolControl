module Web.View.Enrollments.New where
import Web.View.Prelude

data NewView = NewView
    { enrollment  :: Enrollment
    , students    :: [User]
    , disciplinas :: [(Subject, Course)]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Nova matrícula</h1>
        {renderForm enrollment students disciplinas}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                , breadcrumbText "Nova matrícula"
                ]

renderForm :: Enrollment -> [User] -> [(Subject, Course)] -> Html
renderForm enrollment students disciplinas = formFor enrollment [hsx|
    {(selectField #studentId students) { fieldLabel = "Aluno", placeholder = "Selecione o aluno" }}
    {(selectField #subjectId disciplinas) { fieldLabel = "Disciplina", placeholder = "Selecione a disciplina" }}
    {(selectField #status statuses) { fieldLabel = "Status" }}
    {submitButton { label = "Matricular" }}
|]
    where
        statuses = [minBound .. maxBound] :: [EnrollmentStatus]
