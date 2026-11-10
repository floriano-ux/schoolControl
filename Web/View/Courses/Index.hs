module Web.View.Courses.Index where
import Web.View.Prelude

data IndexView = IndexView { courses :: [Course] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {cabecalhoPagina "Cursos" (when (pode Gerenciar Cursos) novoCurso)}
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Nome</th>
                            <th>Carga horária</th>
                            <th></th>
                            {when (pode Gerenciar Cursos) colunasDeAcao}
                        </tr>
                    </thead>
                    <tbody>{forEach courses renderCourse}</tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Cursos" CoursesAction
                ]
            novoCurso = [hsx|<a href={pathTo NewCourseAction} class="btn btn-primary">+ Novo curso</a>|]

renderCourse :: Course -> Html
renderCourse course = [hsx|
    <tr>
        <td>{course.name}</td>
        <td>{course.workload} h</td>
        <td><a href={ShowCourseAction course.id}>Ver</a></td>
        {when (pode Gerenciar Cursos) (celulasDeAcao editar (linkExcluir (pathTo (DeleteCourseAction course.id))))}
    </tr>
|]
    where
        editar = [hsx|<a href={EditCourseAction course.id} class="text-muted">Editar</a>|]
