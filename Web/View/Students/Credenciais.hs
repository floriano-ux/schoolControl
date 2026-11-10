module Web.View.Students.Credenciais where
import Web.View.Prelude

data CredenciaisView = CredenciaisView
    { student :: User
    , senha   :: Text
    }

instance View CredenciaisView where
    html CredenciaisView { .. } = [hsx|
        {breadcrumb}
        <h1>Credenciais de acesso</h1>
        <div class="alert alert-warning">
            Anote agora; ela não será exibida novamente.
        </div>
        <div class="card mb-4">
            <div class="card-body">
                <dl class="row mb-0">
                    <dt class="col-sm-3 text-muted">Estudante</dt>
                    <dd class="col-sm-9">{student.name}</dd>
                    <dt class="col-sm-3 text-muted">E-mail</dt>
                    <dd class="col-sm-9">{student.email}</dd>
                    <dt class="col-sm-3 text-muted">Senha</dt>
                    <dd class="col-sm-9"><code class="fs-5">{senha}</code></dd>
                </dl>
            </div>
        </div>
        <a href={StudentsAction} class="btn btn-primary">Voltar para Estudantes</a>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Estudantes" StudentsAction
                , breadcrumbText "Credenciais"
                ]
