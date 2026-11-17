module Web.View.Courses.Show where
import Web.View.Prelude

data ShowView = ShowView
    { course   :: Course
    , subjects :: [Subject]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{course.name}</h1>
        <dl>
            <dt>Nome</dt><dd>{course.name}</dd>
            <dt>Carga horária</dt><dd>{course.workload} horas</dd>
        </dl>

        <div class="card">
            <div class="card-header">Disciplinas</div>
            <ul class="list-group list-group-flush">
                {forEach subjects renderDisciplina}
                {when (null subjects) semDisciplinas}
            </ul>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Cursos" CoursesAction
                            , breadcrumbText "Detalhes do curso"
                            ]
            semDisciplinas = [hsx|<li class="list-group-item text-muted">Nenhuma disciplina cadastrada</li>|]

renderDisciplina :: Subject -> Html
renderDisciplina subject = [hsx|
    <li class="list-group-item d-flex justify-content-between">
        <a href={ShowSubjectAction subject.id}>{subject.name}</a>
        <span class="text-muted">{subject.workload} h</span>
    </li>
|]
