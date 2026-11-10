module Web.View.Subjects.Show where
import Web.View.Prelude

data ShowView = ShowView
    { subject :: Subject
    , course  :: Course
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{subject.name}</h1>
        <dl>
            <dt>Nome</dt><dd>{subject.name}</dd>
            <dt>Curso</dt><dd><a href={ShowCourseAction course.id}>{course.name}</a></dd>
            <dt>Carga horária</dt><dd>{subject.workload} horas</dd>
        </dl>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Disciplinas" SubjectsAction
                            , breadcrumbText "Detalhes da disciplina"
                            ]
