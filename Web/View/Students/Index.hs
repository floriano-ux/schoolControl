module Web.View.Students.Index where
import Web.View.Prelude

data IndexView = IndexView { students :: [User] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {cabecalhoPagina "Estudantes" (when (pode Gerenciar Estudantes) novoEstudante)}
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Nome</th>
                            <th>E-mail</th>
                            <th></th>
                            {when (pode Gerenciar Estudantes) colunasDeAcao}
                        </tr>
                    </thead>
                    <tbody>{forEach students renderStudent}</tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Estudantes" StudentsAction
                ]
            novoEstudante = [hsx|<a href={pathTo NewStudentAction} class="btn btn-primary">+ Novo estudante</a>|]

renderStudent :: User -> Html
renderStudent student = [hsx|
    <tr>
        <td>{student.name}</td>
        <td>{student.email}</td>
        <td><a href={ShowStudentAction student.id}>Ver</a></td>
        {when (pode Gerenciar Estudantes) (celulasDeAcao editar (linkExcluir (pathTo (DeleteStudentAction student.id))))}
    </tr>
|]
    where
        editar = [hsx|<a href={EditStudentAction student.id} class="text-muted">Editar</a>|]
