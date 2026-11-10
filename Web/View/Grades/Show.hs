module Web.View.Grades.Show where
import Web.View.Prelude

data ShowView = ShowView
    { grade   :: Grade
    , detalhe :: MatriculaDetalhada
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="sga-page-head">
            <h1 class="h3 mb-0">Boletim de notas</h1>
            <div class="sga-page-actions">
                {when (pode Gerenciar Notas) editar}
            </div>
        </div>

        <div class="card mb-4">
            <div class="card-header">
                <strong>{detalhe.aluno.name}</strong> — {detalhe.disciplina.name} / {detalhe.curso.name}
            </div>
            <div class="card-body">
                <div class="row g-3">
                    <div class="col-sm-6 col-lg-3">
                        <div class="card h-100 bg-light border-0">
                            <div class="card-body">
                                <div class="text-muted small">AV1 (70% PE1 + 30% AVf1)</div>
                                <div class="fs-4 fw-bold">{formatarNota (notaAV1 grade)}</div>
                                <div class="small text-muted">PE: {formatarNota grade.pe1} | AVf: {formatarNota grade.avf1}</div>
                            </div>
                        </div>
                    </div>
                    <div class="col-sm-6 col-lg-3">
                        <div class="card h-100 bg-light border-0">
                            <div class="card-body">
                                <div class="text-muted small">AV2 (70% PE2 + 30% AVf2)</div>
                                <div class="fs-4 fw-bold">{formatarNota (notaAV2 grade)}</div>
                                <div class="small text-muted">PE: {formatarNota grade.pe2} | AVf: {formatarNota grade.avf2}</div>
                            </div>
                        </div>
                    </div>
                    <div class="col-sm-6 col-lg-3">
                        <div class="card h-100 bg-light border-0">
                            <div class="card-body">
                                <div class="text-muted small">AV3 (se realizada)</div>
                                <div class="fs-4 fw-bold">{formatarMedia (notaAV3 grade)}</div>
                                <div class="small text-muted">{av3Detalhe}</div>
                            </div>
                        </div>
                    </div>
                    <div class="col-sm-6 col-lg-3">
                        <div class="card h-100 bg-light border-0">
                            <div class="card-body">
                                <div class="text-muted small">Nota Final (NF)</div>
                                <div class="fs-4 fw-bold text-primary">{formatarNota (mediaFinal grade)}</div>
                                <div class="small mt-1 d-flex align-items-center gap-2">
                                    {badgeSituacao (situacaoDaMatricula detalhe.matricula.status grade)}
                                    <span class="fw-semibold">Conceito {show (classificarMedia (mediaFinal grade))}</span>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Notas" GradesAction
                , breadcrumbText "Detalhes da nota"
                ]
            editar = [hsx|<a href={EditGradeAction grade.id} class="btn btn-outline-primary">Editar notas</a>|]
            av3Detalhe = case (grade.pe3, grade.avf3) of
                (Just pe, Just avf) -> "PE: " <> formatarNota pe <> " | AVf: " <> formatarNota avf
                _                   -> "Não realizada"

