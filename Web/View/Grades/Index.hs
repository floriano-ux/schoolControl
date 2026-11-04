module Web.View.Grades.Index where
import Web.View.Prelude

data IndexView = IndexView
    { grades      :: [Grade]
    , enrollments :: [Enrollment]
    , students    :: [Student]
    , courses     :: [Course]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {cabecalhoPagina "Notas" (when (pode Gerenciar Notas) lancarNotas)}
        <p class="text-muted small">
            AVn = 70% PE + 30% AVf · NF = ((AV1 + AV2) / 2 + AV3) / 2 · aprovado com NF ≥ 5,0
        </p>
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover align-middle">
                    <thead>
                        <tr>
                            <th>Aluno</th>
                            <th>Curso</th>
                            <th class="text-end">AV1</th>
                            <th class="text-end">AV2</th>
                            <th class="text-end">AV3</th>
                            <th class="text-end">NF</th>
                            <th>Situação</th>
                            <th>Conceito</th>
                            <th></th>
                            <th></th>
                            <th></th>
                        </tr>
                    </thead>
                    <tbody>{forEach linhas renderLinha}</tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Notas" GradesAction
                ]
            lancarNotas = [hsx|<a href={pathTo NewGradeAction} class="btn btn-primary">+ Lançar notas</a>|]

            -- Junta cada nota com a sua matrícula (mapMaybe descarta notas órfãs)
            linhas = sortOn (\(aluno, curso, _, _) -> (aluno, curso)) (mapMaybe comMatricula grades)
            comMatricula grade = do
                enrollment <- find (\e -> e.id == grade.enrollmentId) enrollments
                pure ( nomeDoAluno students enrollment.studentId
                     , nomeDoCurso courses enrollment.courseId
                     , enrollment
                     , grade
                     )

            renderLinha (aluno, curso, enrollment, grade) = [hsx|
                <tr>
                    <td>{aluno}</td>
                    <td>{curso}</td>
                    <td class="text-end">{formatarNota (notaAV1 grade)}</td>
                    <td class="text-end">{formatarNota (notaAV2 grade)}</td>
                    <td class="text-end">{formatarMedia (notaAV3 grade)}</td>
                    <td class="text-end fw-bold">{formatarNota (mediaFinal grade)}</td>
                    <td>{badgeSituacao (situacaoDaMatricula enrollment.status grade)}</td>
                    <td>{show (classificarMedia (mediaFinal grade))}</td>
                    <td><a href={ShowGradeAction grade.id}>Ver</a></td>
                    <td>{when (pode Gerenciar Notas) (editar grade)}</td>
                    <td>{when (pode Gerenciar Notas) (linkExcluir (pathTo (DeleteGradeAction grade.id)))}</td>
                </tr>
            |]

            editar grade = [hsx|<a href={EditGradeAction grade.id} class="text-muted">Editar</a>|]
