module Web.View.Reports.Index where
import Web.View.Prelude

data IndexView = IndexView
    { medias     :: [(Text, Maybe Double)]
    , ranking    :: [(Int, Text, Double)]
    , alunos     :: [(Text, [Text])]
    , ativas     :: [(Text, Int)]
    , situacoes  :: [(Situacao, Int)]
    , cargaTotal :: Int
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}
        <h1>Relatórios</h1>
        <p class="text-muted">
            Calculados no módulo puro <code>Application.Domain</code> com map, filter, foldr/foldl' e concatMap.
        </p>

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
            breadcrumb = renderBreadcrumb [ breadcrumbLink "Relatórios" ReportsAction ]

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
