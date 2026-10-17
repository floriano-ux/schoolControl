module Web.View.Students.Index where
import Web.View.Prelude

data IndexView = IndexView { students :: [Student] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>
            Estudantes
            {when (pode Gerenciar Estudantes) novoEstudante}
        </h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Nome</th>
                        <th>E-mail</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach students renderStudent}</tbody>
            </table>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Estudantes" StudentsAction
                ]
            novoEstudante = [hsx|<a href={pathTo NewStudentAction} class="btn btn-primary ms-4">+ Novo estudante</a>|]

renderStudent :: Student -> Html
renderStudent student = [hsx|
    <tr>
        <td>{student.name}</td>
        <td>{student.email}</td>
        <td><a href={ShowStudentAction student.id}>Ver</a></td>
        <td>{when (pode Gerenciar Estudantes) editar}</td>
        <td>{when (pode Gerenciar Estudantes) (linkExcluir (pathTo (DeleteStudentAction student.id)))}</td>
    </tr>
|]
    where
        editar = [hsx|<a href={EditStudentAction student.id} class="text-muted">Editar</a>|]
