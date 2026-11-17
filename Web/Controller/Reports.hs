module Web.Controller.Reports where

import Web.Controller.Prelude
import Web.View.Reports.Index

instance Controller ReportsController where
    beforeAction = exigirPermissao Visualizar Relatorios

    -- Busca as tabelas uma vez e monta todos os relatórios com as funções puras do domínio.
    -- 'autoRefresh' reenvia a página (diff via WebSocket) sempre que as tabelas consultadas mudarem.
    action ReportsAction = autoRefresh do
        users       <- query @User |> fetch
        subjects    <- query @Subject |> orderBy #name |> fetch
        todosCursos <- query @Course |> orderBy #name |> fetch
        enrollments <- query @Enrollment |> fetch
        grades      <- query @Grade |> fetch

        let filtro = FiltroRelatorio
                { cursoId      = paramOrNothing @(Id Course) "courseId"
                , disciplinaId = paramOrNothing @(Id Subject) "subjectId"
                , status       = paramOrNothing @EnrollmentStatus "status"
                                 <|> (paramOrNothing @Text "status" >>= parseStatus)
                }

            usuario              = currentUser
            matriculas           = matriculasVisiveis usuario (detalharMatriculas users subjects todosCursos enrollments)
            disciplinas          = sortOn rotuloDaDisciplina (disciplinasComCurso todosCursos (disciplinasVisiveis usuario subjects))
            courses              = cursosVisiveis usuario subjects todosCursos
            matriculasFiltradas  = aplicarFiltro filtro matriculas
            cursosFiltrados      = filtrarCursos filtro.cursoId courses
            disciplinasFiltradas = filtrarDisciplinas filtro disciplinas
            linhasBoletim        = boletim matriculasFiltradas grades

            medias           = mediaPorCurso cursosFiltrados matriculasFiltradas grades
            mediasDisciplina = mediaPorDisciplina disciplinasFiltradas matriculasFiltradas grades
            ranking          = rankingDeAlunos matriculasFiltradas grades
            alunosNoCurso    = alunosPorCurso cursosFiltrados matriculasFiltradas
            ativas           = matriculasAtivasPorCurso cursosFiltrados matriculasFiltradas
            situacoes        = contarPorSituacao linhasBoletim
            cargaTotal       = cargaHorariaTotal cursosFiltrados

            totalAlunos      = totalAlunosFiltrados filtro (alunosVisiveis usuario matriculas (alunos users)) matriculasFiltradas
            matriculasAtivas = totalMatriculasAtivas matriculasFiltradas
            taxaAprov        = taxaAprovacao linhasBoletim

        render IndexView { .. }
