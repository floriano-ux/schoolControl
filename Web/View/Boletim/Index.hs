module Web.View.Boletim.Index where
import Web.View.Prelude

data IndexView = IndexView { linhas :: [(MatriculaDetalhada, Maybe Grade)] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {cabecalhoPagina "Meu boletim" mempty}
        <p class="text-muted small">
            AVn = 70% PE + 30% AVf · NF = ((AV1 + AV2) / 2 + AV3) / 2 · aprovado com NF ≥ 5,0
        </p>

        <div class="row g-3 mb-4">
            <div class="col-sm-6 col-lg-4">
                <div class="card h-100">
                    <div class="card-body">
                        <div class="text-muted small text-uppercase fw-semibold">Disciplinas em curso</div>
                        <div class="fs-2 fw-bold text-success mt-1">{emCurso}</div>
                    </div>
                </div>
            </div>
            <div class="col-sm-6 col-lg-4">
                <div class="card h-100">
                    <div class="card-body">
                        <div class="text-muted small text-uppercase fw-semibold">Média geral</div>
                        <div class="fs-2 fw-bold text-primary mt-1">{formatarMedia (mediaGeral linhas)}</div>
                    </div>
                </div>
            </div>
        </div>

        <div class="card">
            <div class="table-responsive">
                <table class="table table-hover align-middle mb-0">
                    <thead>
                        <tr>
                            <th>Disciplina</th>
                            <th>Curso</th>
                            <th>Matrícula</th>
                            <th class="text-end">AV1</th>
                            <th class="text-end">AV2</th>
                            <th class="text-end">AV3</th>
                            <th class="text-end">NF</th>
                            <th>Situação</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach linhas renderLinha}
                        {when (null linhas) semMatriculas}
                    </tbody>
                </table>
            </div>
        </div>
    |]
        where
            emCurso = length [ () | (m, _) <- linhas, m.matricula.status == Ativa ]
            semMatriculas = [hsx|<tr><td colspan="8" class="text-muted">Você ainda não está matriculado em nenhuma disciplina.</td></tr>|]

renderLinha :: (MatriculaDetalhada, Maybe Grade) -> Html
renderLinha (m, nota) = [hsx|
    <tr>
        <td>{m.disciplina.name}</td>
        <td>{m.curso.name}</td>
        <td>{badgeStatus m.matricula.status}</td>
        {notas nota}
    </tr>
|]
    where
        notas (Just grade) = [hsx|
            <td class="text-end">{formatarNota (notaAV1 grade)}</td>
            <td class="text-end">{formatarNota (notaAV2 grade)}</td>
            <td class="text-end">{formatarMedia (notaAV3 grade)}</td>
            <td class="text-end fw-bold">{formatarNota (mediaFinal grade)}</td>
            <td>{badgeSituacao (situacaoDaMatricula m.matricula.status grade)}</td>
        |]
        notas Nothing = [hsx|<td colspan="4" class="text-end text-muted">—</td><td class="text-muted">Notas não lançadas</td>|]
