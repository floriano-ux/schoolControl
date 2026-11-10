module Web.View.Users.Show where
import Web.View.Prelude

data ShowView = ShowView
    { user      :: User
    , documento   :: Maybe Text
    , disciplinas :: [(Subject, Course)]
    , professores :: [User]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="sga-page-head">
            <h1 class="h3 mb-0">{user.name}</h1>
            <div class="sga-page-actions">
                <a href={EditUserAction user.id} class="btn btn-outline-primary">Editar usuário</a>
            </div>
        </div>

        <div class="card mb-4">
            <div class="card-body">
                <dl class="row mb-0">
                    <dt class="col-sm-3 text-muted">Nome</dt>
                    <dd class="col-sm-9 fw-semibold">{user.name}</dd>

                    <dt class="col-sm-3 text-muted">E-mail</dt>
                    <dd class="col-sm-9">{user.email}</dd>

                    <dt class="col-sm-3 text-muted">Papel</dt>
                    <dd class="col-sm-9"><span class="badge bg-secondary">{nomeDoPapel user.userRole}</span></dd>

                    <dt class="col-sm-3 text-muted">Documento (CPF)</dt>
                    <dd class="col-sm-9">{fromMaybe "Não informado" documento}</dd>
                </dl>
            </div>
        </div>
        {when (user.userRole == Professor) disciplinasMinistradas}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Usuários" UsersAction
                , breadcrumbText "Detalhes do usuário"
                ]
            disciplinasMinistradas = [hsx|
                <div class="card">
                    <div class="card-header">Disciplinas que ministra</div>
                    <div class="card-body">
                        <p class="text-muted small">
                            Marque as disciplinas deste professor. Uma disciplina tem um único professor:
                            marcar uma disciplina de outro professor transfere a disciplina para este.
                        </p>
                        <form method="POST" action={AtribuirDisciplinasAction user.id}>
                            {forEach disciplinas opcaoDisciplina}
                            {when (null disciplinas) nenhuma}
                            <button type="submit" class="btn btn-primary mt-3">Salvar disciplinas</button>
                        </form>
                    </div>
                </div>
            |]
            opcaoDisciplina par@(subject, _) = [hsx|
                <div class="form-check">
                    <input class="form-check-input" type="checkbox" name="subjectIds"
                           id={"disciplina-" <> tshow subject.id} value={inputValue subject.id}
                           checked={subject.professorId == Just user.id}/>
                    <label class="form-check-label" for={"disciplina-" <> tshow subject.id}>
                        {rotuloDaDisciplina par} {responsavelAtual subject}
                    </label>
                </div>
            |]
            responsavelAtual subject
                | subject.professorId == Just user.id = mempty
                | isNothing subject.professorId       = mempty
                | otherwise = [hsx|<small class="text-muted">(atual: {nomeDoProfessor professores subject})</small>|]
            nenhuma = [hsx|<p class="text-muted mb-0">Nenhuma disciplina cadastrada.</p>|]
