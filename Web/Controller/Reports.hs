module Web.Controller.Reports where

import Web.Controller.Prelude
import Web.View.Reports.Index

instance Controller ReportsController where
    beforeAction = exigirPermissao Visualizar Relatorios

    -- Busca as tabelas uma vez e monta todos os relatórios com as funções puras do domínio.
    -- 'autoRefresh' ativa a reatividade nativa do IHP: monitora as tabelas consultadas
    -- (Student, Course, Enrollment, Grade) e envia diffs DOM via WebSocket (morphdom)
    -- sempre que o banco de dados mudar, sem necessidade de JS ou Miso/Obelisk.
    action ReportsAction = do
        autoRefresh
        students    <- query @Student |> fetch
        courses     <- query @Course |> orderBy #name |> fetch
        enrollments <- query @Enrollment |> fetch
        grades      <- query @Grade |> fetch

        let filtro = FiltroRelatorio
                { cursoId = paramOrNothing @(Id Course) "courseId"
                , status  = paramOrNothing @EnrollmentStatus "status"
                            <|> (paramOrNothing @Text "status" >>= parseStatus)
                }

            matriculasFiltradas = aplicarFiltro filtro enrollments
            cursosFiltrados     = filtrarCursos filtro.cursoId courses
            linhasBoletim       = boletim students matriculasFiltradas grades

            medias      = mediaPorCurso cursosFiltrados matriculasFiltradas grades
            ranking     = rankingDeAlunos students matriculasFiltradas grades
            alunos      = alunosPorCurso cursosFiltrados students matriculasFiltradas
            ativas      = matriculasAtivasPorCurso cursosFiltrados matriculasFiltradas
            situacoes   = contarPorSituacao linhasBoletim
            cargaTotal  = cargaHorariaTotal cursosFiltrados

            totalAlunos      = totalAlunosFiltrados filtro students matriculasFiltradas
            matriculasAtivas = totalMatriculasAtivas matriculasFiltradas
            taxaAprov        = taxaAprovacao linhasBoletim

        render IndexView { .. }
