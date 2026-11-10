module Web.View.Students.Index where
import Web.View.Prelude

data IndexView = IndexView
    { linhas       :: [(User, [Text])]
    , disciplinas  :: [(Subject, Course)]
    , disciplinaId :: Maybe (Id Subject)
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {cabecalhoPagina titulo (when (pode Gerenciar Estudantes) novoEstudante)}
        <div class="card mb-4">
            <div class="card-body">
                <form method="GET" action={StudentsAction} class="row g-2 align-items-end">
                    <div class="col-md-8">
                        <label class="form-label" for="filtroDisciplina">Disciplina</label>
                        <select id="filtroDisciplina" name="subjectId" class="form-select"
                                onchange="this.form.requestSubmit ? this.form.requestSubmit() : this.form.submit()">
                            {opcaoTodas}
                            {forEach disciplinas opcaoDisciplina}
                        </select>
                    </div>
                    <div class="col-md-4 d-flex gap-2">
                        <button type="submit" class="btn btn-primary flex-fill">Filtrar</button>
                        {when (isJust disciplinaId) limpar}
                    </div>
                </form>
            </div>
        </div>
        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Nome</th>
                            <th>E-mail</th>
                            <th>Disciplinas</th>
                            <th></th>
                            {when (pode Gerenciar Estudantes) colunasDeAcao}
                        </tr>
                    </thead>
                    <tbody>
                        {forEach linhas renderStudent}
                        {when (null linhas) nenhum}
                    </tbody>
                </table>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink (toHtml titulo) StudentsAction
                ]
            titulo = maybe "Estudantes" (\u -> nomeDoRecurso u.userRole Estudantes) (currentUserOrNothing @User)
            novoEstudante = [hsx|<a href={pathTo NewStudentAction} class="btn btn-primary">+ Novo estudante</a>|]
            limpar = [hsx|<a href={StudentsAction} class="btn btn-outline-secondary">Limpar</a>|]
            nenhum = [hsx|<tr><td colspan="6" class="text-muted">Nenhum aluno encontrado.</td></tr>|]

            opcaoTodas
                | isNothing disciplinaId = [hsx|<option value="" selected="selected">Todas as disciplinas</option>|]
                | otherwise              = [hsx|<option value="">Todas as disciplinas</option>|]

            opcaoDisciplina par@(subject, _)
                | disciplinaId == Just subject.id =
                    [hsx|<option value={inputValue subject.id} selected="selected">{rotuloDaDisciplina par}</option>|]
                | otherwise =
                    [hsx|<option value={inputValue subject.id}>{rotuloDaDisciplina par}</option>|]

renderStudent :: (User, [Text]) -> Html
renderStudent (student, nomesDasDisciplinas) = [hsx|
    <tr>
        <td>{student.name}</td>
        <td>{student.email}</td>
        <td>{forEach nomesDasDisciplinas badgeDisciplina}{when (null nomesDasDisciplinas) semDisciplina}</td>
        <td><a href={ShowStudentAction student.id}>Ver</a></td>
        {when (pode Gerenciar Estudantes) (celulasDeAcao editar (linkExcluir (pathTo (DeleteStudentAction student.id))))}
    </tr>
|]
    where
        editar = [hsx|<a href={EditStudentAction student.id} class="text-muted">Editar</a>|]
        badgeDisciplina nome = [hsx|<span class="badge bg-light text-dark border me-1">{nome}</span>|]
        semDisciplina = [hsx|<span class="text-muted small">—</span>|]
