module Web.View.Users.Show where
import Web.View.Prelude

data ShowView = ShowView
    { user      :: User
    , documento :: Maybe Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="sga-page-head">
            <h1 class="h3 mb-0">{user.name}</h1>
            <div class="sga-page-actions">
                <a href={EditUserAction user.id} class="btn btn-outline-primary">Editar usuário</a>
            </div>
        </div>

        <div class="card mb-4">
            <div class="card-body">
                <dl class="row mb-0">
                    <dt class="col-sm-3 text-muted">Nome</dt>
                    <dd class="col-sm-9 fw-semibold">{user.name}</dd>

                    <dt class="col-sm-3 text-muted">E-mail</dt>
                    <dd class="col-sm-9">{user.email}</dd>

                    <dt class="col-sm-3 text-muted">Papel</dt>
                    <dd class="col-sm-9"><span class="badge bg-secondary">{nomeDoPapel user.userRole}</span></dd>

                    <dt class="col-sm-3 text-muted">Documento (CPF)</dt>
                    <dd class="col-sm-9">{fromMaybe "Não informado" documento}</dd>
                </dl>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                , breadcrumbText "Detalhes do usuário"
                ]

