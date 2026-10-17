module Web.View.Courses.Show where
import Web.View.Prelude

data ShowView = ShowView { course :: Course }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{course.name}</h1>
        <dl>
            <dt>Nome</dt><dd>{course.name}</dd>
            <dt>Carga horária</dt><dd>{course.workload} horas</dd>
        </dl>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Cursos" CoursesAction
                            , breadcrumbText "Detalhes do curso"
                            ]
