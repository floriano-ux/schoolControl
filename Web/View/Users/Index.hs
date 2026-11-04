module Web.View.Users.Index where
import Web.View.Prelude

data IndexView = IndexView { users :: [User] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {cabecalhoPagina "Usuários" novoUsuario}
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Nome</th>
                            <th>E-mail</th>
                            <th>Papel</th>
                            <th>CPF</th>
                            <th></th>
                            <th></th>
                            <th></th>
                        </tr>
                    </thead>
                    <tbody>{forEach users renderUser}</tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                ]
            novoUsuario = [hsx|<a href={pathTo NewUserAction} class="btn btn-primary">+ Novo usuário</a>|]

renderUser :: User -> Html
renderUser user = [hsx|
    <tr>
        <td>{user.name}</td>
        <td>{user.email}</td>
        <td><span class="badge bg-secondary">{nomeDoPapel user.userRole}</span></td>
        <td>{maybe "—" (const "••• cadastrado") user.document :: Text}</td>
        <td><a href={ShowUserAction user.id}>Ver</a></td>
        <td><a href={EditUserAction user.id} class="text-muted">Editar</a></td>
        <td>{linkExcluir (pathTo (DeleteUserAction user.id))}</td>
    </tr>
|]
