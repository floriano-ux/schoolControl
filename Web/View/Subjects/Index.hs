module Web.View.Subjects.Index where
import Web.View.Prelude

data IndexView = IndexView
    { subjects :: [Subject]
    , courses  :: [Course]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {cabecalhoPagina "Disciplinas" (when (pode Gerenciar Disciplinas) novaDisciplina)}
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Nome</th>
                            <th>Curso</th>
                            <th>Carga horária</th>
                            <th></th>
                            {when (pode Gerenciar Disciplinas) colunasDeAcao}
                        </tr>
                    </thead>
                    <tbody>{forEach linhas renderLinha}</tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Disciplinas" SubjectsAction
                ]
            novaDisciplina = [hsx|<a href={pathTo NewSubjectAction} class="btn btn-primary">+ Nova disciplina</a>|]

            linhas = sortOn (\(s, c) -> (c.name, s.name)) (disciplinasComCurso courses subjects)

            renderLinha (subject, course) = [hsx|
                <tr>
                    <td>{subject.name}</td>
                    <td>{course.name}</td>
                    <td>{subject.workload} h</td>
                    <td><a href={ShowSubjectAction subject.id}>Ver</a></td>
                    {when (pode Gerenciar Disciplinas) (celulasDeAcao (editar subject) (linkExcluir (pathTo (DeleteSubjectAction subject.id))))}
                </tr>
            |]

            editar subject = [hsx|<a href={EditSubjectAction subject.id} class="text-muted">Editar</a>|]
