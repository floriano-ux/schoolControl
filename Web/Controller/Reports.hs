module Web.Controller.Reports where

import Web.Controller.Prelude
import Web.View.Reports.Index

instance Controller ReportsController where
    beforeAction = exigirPermissao Visualizar Relatorios

    -- Busca as tabelas uma vez e monta todos os relatórios com as funções puras do domínio.
    action ReportsAction = do
        students    <- query @Student |> fetch
        courses     <- query @Course |> orderBy #name |> fetch
        enrollments <- query @Enrollment |> fetch
        grades      <- query @Grade |> fetch
        let medias      = mediaPorCurso courses enrollments grades
            ranking     = rankingDeAlunos students enrollments grades
            alunos      = alunosPorCurso courses students enrollments
            ativas      = matriculasAtivasPorCurso courses enrollments
            situacoes   = contarPorSituacao (boletim students enrollments grades)
            cargaTotal  = cargaHorariaTotal courses
        render IndexView { .. }
