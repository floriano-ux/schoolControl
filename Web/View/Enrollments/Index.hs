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

        {cabecalhoPagina "Matrículas" (when (pode Gerenciar Matriculas) novaMatricula)}
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Aluno</th>
                            <th>Curso</th>
                            <th>Status</th>
                            <th></th>
                            <th></th>
                            <th></th>
                        </tr>
                    </thead>
                    <tbody>{forEach linhas renderLinha}</tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                ]
            novaMatricula = [hsx|<a href={pathTo NewEnrollmentAction} class="btn btn-primary">+ Nova matrícula</a>|]

            -- "JOIN" em memória e ordenação por aluno e curso
            linhas = sortOn (\(aluno, curso, _) -> (aluno, curso))
                [ (nomeDoAluno students e.studentId, nomeDoCurso courses e.courseId, e) | e <- enrollments ]

            renderLinha (aluno, curso, enrollment) = [hsx|
                <tr>
                    <td>{aluno}</td>
                    <td>{curso}</td>
                    <td>{badgeStatus enrollment.status}</td>
                    <td><a href={ShowEnrollmentAction enrollment.id}>Ver</a></td>
                    <td>{when (pode Gerenciar Matriculas) (editar enrollment)}</td>
                    <td>{when (pode Gerenciar Matriculas) (linkExcluir (pathTo (DeleteEnrollmentAction enrollment.id)))}</td>
                </tr>
            |]

            editar enrollment = [hsx|<a href={EditEnrollmentAction enrollment.id} class="text-muted">Editar</a>|]
