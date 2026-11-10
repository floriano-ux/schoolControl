module Web.View.Enrollments.Show where
import Web.View.Prelude

data ShowView = ShowView { detalhe :: MatriculaDetalhada }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="sga-page-head">
            <h1 class="h3 mb-0">Detalhes da matrícula</h1>
            <div class="sga-page-actions">
                {when (pode Gerenciar Matriculas) editar}
            </div>
        </div>

        <div class="card mb-4">
            <div class="card-body">
                <dl class="row mb-0">
                    <dt class="col-sm-3 text-muted">Aluno</dt>
                    <dd class="col-sm-9 fw-semibold">{detalhe.aluno.name} <small class="text-muted">({detalhe.aluno.email})</small></dd>

                    <dt class="col-sm-3 text-muted">Disciplina</dt>
                    <dd class="col-sm-9">{detalhe.disciplina.name} <small class="text-muted">({detalhe.disciplina.workload} h)</small></dd>

                    <dt class="col-sm-3 text-muted">Curso</dt>
                    <dd class="col-sm-9">{detalhe.curso.name}</dd>

                    <dt class="col-sm-3 text-muted">Status</dt>
                    <dd class="col-sm-9">{badgeStatus detalhe.matricula.status}</dd>
                </dl>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                , breadcrumbText "Detalhes da matrícula"
                ]
            editar = [hsx|<a href={EditEnrollmentAction detalhe.matricula.id} class="btn btn-outline-primary">Editar matrícula</a>|]

