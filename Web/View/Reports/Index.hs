module Web.View.Reports.Index where
import Web.View.Prelude

data IndexView = IndexView
    { medias           :: [(Text, Maybe Double)]
    , ranking          :: [(Int, Text, Double)]
    , alunos           :: [(Text, [Text])]
    , ativas           :: [(Text, Int)]
    , situacoes        :: [(Situacao, Int)]
    , cargaTotal       :: Int
    , courses          :: [Course]
    , filtro           :: FiltroRelatorio
    , totalAlunos      :: Int
    , matriculasAtivas :: Int
    , taxaAprov        :: Maybe Double
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}
        <div class="sga-page-head">
            <div>
                <h1 class="h2 mb-1">Dashboard</h1>
                <p class="text-muted mb-0">
                    Acompanhamento acadêmico com médias, ranking e filtros dinâmicos calculados puramente em Haskell.
                </p>
            </div>
        </div>

        <!-- Faixa de KPIs -->
        <div class="row g-3 mb-4">
            <div class="col-sm-6 col-lg-3">
                <div class="card h-100">
                    <div class="card-body">
                        <div class="text-muted small text-uppercase fw-semibold">Total de Alunos</div>
                        <div class="fs-2 fw-bold text-dark mt-1">{totalAlunos}</div>
                        <div class="text-muted small mt-1">{subtextoAlunos}</div>
                    </div>
                </div>
            </div>
            <div class="col-sm-6 col-lg-3">
                <div class="card h-100">
                    <div class="card-body">
                        <div class="text-muted small text-uppercase fw-semibold">Matrículas Ativas</div>
                        <div class="fs-2 fw-bold text-success mt-1">{matriculasAtivas}</div>
                        <div class="text-muted small mt-1">em andamento regular</div>
                    </div>
                </div>
            </div>
            <div class="col-sm-6 col-lg-3">
                <div class="card h-100">
                    <div class="card-body">
                        <div class="text-muted small text-uppercase fw-semibold">Carga Total</div>
                        <div class="fs-2 fw-bold text-dark mt-1">{cargaTotal} h</div>
                        <div class="text-muted small mt-1">cursos considerados</div>
                    </div>
                </div>
            </div>
            <div class="col-sm-6 col-lg-3">
                <div class="card h-100">
                    <div class="card-body">
                        <div class="text-muted small text-uppercase fw-semibold">% Aprovados</div>
                        <div class="fs-2 fw-bold text-primary mt-1">{formatarTaxa taxaAprov}</div>
                        <div class="text-muted small mt-1">com notas lançadas</div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Formulário de Filtro (GET) -->
        <div class="card mb-4">
            <div class="card-body">
                <form method="GET" action={ReportsAction} class="row g-3 align-items-end">
                    <div class="col-md-5">
                        <label class="form-label" for="filtroCurso">Curso</label>
                        <select id="filtroCurso" name="courseId" class="form-select">
                            {renderTodosCursos}
                            {forEach courses renderOpcaoCurso}
                        </select>
                    </div>
                    <div class="col-md-4">
                        <label class="form-label" for="filtroStatus">Status da matrícula</label>
                        <select id="filtroStatus" name="status" class="form-select">
                            {renderTodosStatus}
                            {forEach statuses renderOpcaoStatus}
                        </select>
                    </div>
                    <div class="col-md-3 d-flex gap-2">
                        <button type="submit" class="btn btn-primary flex-fill">
                            Filtrar
                        </button>
                        {when filtroAtivo linkLimpar}
                    </div>
                </form>
            </div>
        </div>

        <!-- Cards de agregação -->
        <div class="row g-4">
            <div class="col-lg-6">
                <div class="card h-100">
                    <div class="card-header">Média final por curso</div>
                    <table class="table mb-0">
                        <thead><tr><th>Curso</th><th class="text-end">Média (NF)</th></tr></thead>
                        <tbody>{forEach medias renderMedia}</tbody>
                    </table>
                    <div class="card-footer text-muted small">Carga horária total dos cursos: {cargaTotal} h</div>
                </div>
            </div>

            <div class="col-lg-6">
                <div class="card h-100">
                    <div class="card-header">Matrículas ativas por curso</div>
                    <table class="table mb-0">
                        <thead><tr><th>Curso</th><th class="text-end">Ativas</th></tr></thead>
                        <tbody>{forEach ativas renderAtivas}</tbody>
                    </table>
                </div>
            </div>

            <div class="col-lg-6">
                <div class="card h-100">
                    <div class="card-header">Situação dos alunos (boletim)</div>
                    <table class="table mb-0">
                        <tbody>{forEach situacoes renderSituacao}</tbody>
                    </table>
                </div>
            </div>

            {when (pode Visualizar Estudantes) (rankingCard ranking)}
            {when (pode Visualizar Estudantes) (alunosCard alunos)}
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb [ breadcrumbLink "Dashboard" ReportsAction ]

            subtextoAlunos :: Text
            subtextoAlunos
                | filtroAtivo = "no recorte filtrado"
                | otherwise   = "cadastrados no sistema"

            formatarTaxa :: Maybe Double -> Text
            formatarTaxa Nothing  = "—"
            formatarTaxa (Just p) = formatarNota p <> "%"

            renderTodosCursos =
                if isNothing filtro.cursoId
                    then [hsx|<option value="" selected="selected">Todos os cursos</option>|]
                    else [hsx|<option value="">Todos os cursos</option>|]

            renderOpcaoCurso course =
                if filtro.cursoId == Just course.id
                    then [hsx|<option value={tshow course.id} selected="selected">{course.name}</option>|]
                    else [hsx|<option value={tshow course.id}>{course.name}</option>|]

            renderTodosStatus =
                if isNothing filtro.status
                    then [hsx|<option value="" selected="selected">Todos os status</option>|]
                    else [hsx|<option value="">Todos os status</option>|]

            renderOpcaoStatus st =
                if filtro.status == Just st
                    then [hsx|<option value={inputValue st} selected="selected">{nomeDoStatus st}</option>|]
                    else [hsx|<option value={inputValue st}>{nomeDoStatus st}</option>|]

            statuses = [minBound .. maxBound] :: [EnrollmentStatus]
            filtroAtivo = isJust filtro.cursoId || isJust filtro.status
            linkLimpar = [hsx|<a href={ReportsAction} class="btn btn-outline-secondary">Limpar</a>|]

renderMedia :: (Text, Maybe Double) -> Html
renderMedia (curso, media) = [hsx|
    <tr><td>{curso}</td><td class="text-end">{formatarMedia media}</td></tr>
|]

renderAtivas :: (Text, Int) -> Html
renderAtivas (curso, quantidade) = [hsx|
    <tr><td>{curso}</td><td class="text-end">{quantidade}</td></tr>
|]

renderSituacao :: (Situacao, Int) -> Html
renderSituacao (situacao, quantidade) = [hsx|
    <tr><td>{nomeDaSituacao situacao}</td><td class="text-end">{quantidade}</td></tr>
|]

-- Nomes de alunos só aparecem para quem pode ver Estudantes (o Aluno vê apenas números agregados).
rankingCard :: [(Int, Text, Double)] -> Html
rankingCard ranking = [hsx|
    <div class="col-lg-6">
        <div class="card h-100">
            <div class="card-header">Ranking geral dos alunos</div>
            <table class="table mb-0">
                <thead><tr><th>#</th><th>Aluno</th><th class="text-end">Média geral</th></tr></thead>
                <tbody>{forEach ranking renderPosicao}</tbody>
            </table>
        </div>
    </div>
|]
    where
        renderPosicao (posicao, nome, media) = [hsx|
            <tr><td>{posicao}º</td><td>{nome}</td><td class="text-end">{formatarNota media}</td></tr>
        |]

alunosCard :: [(Text, [Text])] -> Html
alunosCard alunos = [hsx|
    <div class="col-12">
        <div class="card">
            <div class="card-header">Alunos por curso <small class="text-muted">(sem matrículas trancadas)</small></div>
            <ul class="list-group list-group-flush">{forEach alunos renderCurso}</ul>
        </div>
    </div>
|]
    where
        renderCurso (curso, nomes) = [hsx|
            <li class="list-group-item">
                <strong>{curso}</strong>
                <span class="badge bg-primary ms-2">{length nomes}</span>
                <div class="text-muted small">{listaDeNomes nomes}</div>
            </li>
        |]
        listaDeNomes :: [Text] -> Text
        listaDeNomes [] = "Nenhum aluno matriculado"
        listaDeNomes nomes = intercalate ", " nomes
