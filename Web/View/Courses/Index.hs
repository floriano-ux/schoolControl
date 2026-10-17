module Web.View.Courses.Index where
import Web.View.Prelude

data IndexView = IndexView { courses :: [Course] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>
            Cursos
            {when (pode Gerenciar Cursos) novoCurso}
        </h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Nome</th>
                        <th>Carga horária</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach courses renderCourse}</tbody>
            </table>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Cursos" CoursesAction
                ]
            novoCurso = [hsx|<a href={pathTo NewCourseAction} class="btn btn-primary ms-4">+ Novo curso</a>|]

renderCourse :: Course -> Html
renderCourse course = [hsx|
    <tr>
        <td>{course.name}</td>
        <td>{course.workload} h</td>
        <td><a href={ShowCourseAction course.id}>Ver</a></td>
        <td>{when (pode Gerenciar Cursos) editar}</td>
        <td>{when (pode Gerenciar Cursos) (linkExcluir (pathTo (DeleteCourseAction course.id)))}</td>
    </tr>
|]
    where
        editar = [hsx|<a href={EditCourseAction course.id} class="text-muted">Editar</a>|]
