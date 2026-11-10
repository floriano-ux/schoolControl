module Web.View.Enrollments.Edit where
import Web.View.Prelude

data EditView = EditView { detalhe :: MatriculaDetalhada }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar matrícula</h1>
        <dl>
            <dt>Aluno</dt><dd>{detalhe.aluno.name}</dd>
            <dt>Disciplina</dt><dd>{detalhe.disciplina.name}</dd>
            <dt>Curso</dt><dd>{detalhe.curso.name}</dd>
        </dl>
        {renderForm detalhe.matricula}
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
