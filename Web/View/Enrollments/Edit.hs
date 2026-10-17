module Web.View.Enrollments.Edit where
import Web.View.Prelude

data EditView = EditView
    { enrollment :: Enrollment
    , student    :: Student
    , course     :: Course
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar matrícula</h1>
        <dl>
            <dt>Aluno</dt><dd>{student.name}</dd>
            <dt>Curso</dt><dd>{course.name}</dd>
        </dl>
        {renderForm enrollment}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                , breadcrumbText "Editar matrícula"
                ]

renderForm :: Enrollment -> Html
renderForm enrollment = formFor enrollment [hsx|
    {(selectField #status statuses) { fieldLabel = "Status" }}
    {submitButton { label = "Salvar alterações" }}
|]
    where
        statuses = [minBound .. maxBound] :: [EnrollmentStatus]
