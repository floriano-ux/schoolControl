module Web.View.Users.Index where
import Web.View.Prelude

data IndexView = IndexView { users :: [User] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Usuários<a href={pathTo NewUserAction} class="btn btn-primary ms-4">+ Novo usuário</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Nome</th>
                        <th>E-mail</th>
                        <th>Papel</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach users renderUser}</tbody>
            </table>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                ]

renderUser :: User -> Html
renderUser user = [hsx|
    <tr>
        <td>{user.name}</td>
        <td>{user.email}</td>
        <td><span class="badge bg-secondary">{nomeDoPapel user.userRole}</span></td>
        <td><a href={EditUserAction user.id} class="text-muted">Editar</a></td>
        <td>{linkExcluir (pathTo (DeleteUserAction user.id))}</td>
    </tr>
|]
