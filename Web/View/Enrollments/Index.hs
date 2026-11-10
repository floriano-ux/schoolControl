module Web.View.Enrollments.Index where
import Web.View.Prelude

data IndexView = IndexView { matriculas :: [MatriculaDetalhada] }

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
                            <th>Disciplina</th>
                            <th>Curso</th>
                            <th>Status</th>
                            <th></th>
                            {when (pode Gerenciar Matriculas) colunasDeAcao}
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

            linhas = sortOn (\m -> (m.aluno.name, m.curso.name, m.disciplina.name)) matriculas

            renderLinha m = [hsx|
                <tr>
                    <td>{m.aluno.name}</td>
                    <td>{m.disciplina.name}</td>
                    <td>{m.curso.name}</td>
                    <td>{badgeStatus m.matricula.status}</td>
                    <td><a href={ShowEnrollmentAction m.matricula.id}>Ver</a></td>
                    {when (pode Gerenciar Matriculas) (celulasDeAcao (editar m.matricula) (linkExcluir (pathTo (DeleteEnrollmentAction m.matricula.id))))}
                </tr>
            |]

            editar enrollment = [hsx|<a href={EditEnrollmentAction enrollment.id} class="text-muted">Editar</a>|]
