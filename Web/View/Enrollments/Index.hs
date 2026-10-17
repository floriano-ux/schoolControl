module Web.View.Enrollments.Index where
import Web.View.Prelude

data IndexView = IndexView
    { enrollments :: [Enrollment]
    , students    :: [Student]
    , courses     :: [Course]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>
            Matrículas
            {when (pode Gerenciar Matriculas) novaMatricula}
        </h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Aluno</th>
                        <th>Curso</th>
                        <th>Status</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach linhas renderLinha}</tbody>
            </table>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                ]
            novaMatricula = [hsx|<a href={pathTo NewEnrollmentAction} class="btn btn-primary ms-4">+ Nova matrícula</a>|]

            -- "JOIN" em memória e ordenação por aluno e curso
            linhas = sortOn (\(aluno, curso, _) -> (aluno, curso))
                [ (nomeDoAluno students e.studentId, nomeDoCurso courses e.courseId, e) | e <- enrollments ]

            renderLinha (aluno, curso, enrollment) = [hsx|
                <tr>
                    <td>{aluno}</td>
                    <td>{curso}</td>
                    <td>{nomeDoStatus enrollment.status}</td>
                    <td>{when (pode Gerenciar Matriculas) (editar enrollment)}</td>
                    <td>{when (pode Gerenciar Matriculas) (linkExcluir (pathTo (DeleteEnrollmentAction enrollment.id)))}</td>
                </tr>
            |]

            editar enrollment = [hsx|<a href={EditEnrollmentAction enrollment.id} class="text-muted">Editar</a>|]
