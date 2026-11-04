module Web.View.Enrollments.Show where
import Web.View.Prelude

data ShowView = ShowView
    { enrollment :: Enrollment
    , student    :: Student
    , course     :: Course
    }

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
                    <dd class="col-sm-9 fw-semibold">{student.name} <small class="text-muted">({student.email})</small></dd>

                    <dt class="col-sm-3 text-muted">Curso</dt>
                    <dd class="col-sm-9">{course.name} <small class="text-muted">({course.workload} h)</small></dd>

                    <dt class="col-sm-3 text-muted">Status</dt>
                    <dd class="col-sm-9">{badgeStatus enrollment.status}</dd>
                </dl>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Matrículas" EnrollmentsAction
                , breadcrumbText "Detalhes da matrícula"
                ]
            editar = [hsx|<a href={EditEnrollmentAction enrollment.id} class="btn btn-outline-primary">Editar matrícula</a>|]

