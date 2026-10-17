module Web.Controller.Grades where

import Web.Controller.Prelude
import Web.View.Grades.Index
import Web.View.Grades.New
import Web.View.Grades.Edit

instance Controller GradesController where
    beforeAction = exigirPermissao Visualizar Notas

    action GradesAction = do
        grades      <- query @Grade |> fetch
        enrollments <- query @Enrollment |> fetch
        students    <- query @Student |> orderBy #name |> fetch
        courses     <- query @Course |> orderBy #name |> fetch
        render IndexView { .. }

    action NewGradeAction = do
        exigirPermissao Gerenciar Notas
        let grade = newRecord @Grade
        opcoes <- carregarOpcoesDeMatricula
        render NewView { .. }

    action CreateGradeAction = do
        exigirPermissao Gerenciar Notas
        let grade = newRecord @Grade
        grade
            |> buildGrade
            |> validarUmaNotaPorMatricula
            >>= ifValid \case
                Left grade -> do
                    opcoes <- carregarOpcoesDeMatricula
                    render NewView { .. }
                Right grade -> do
                    grade |> createRecord
                    setSuccessMessage "Notas lançadas"
                    redirectTo GradesAction

    action EditGradeAction { gradeId } = do
        exigirPermissao Gerenciar Notas
        grade <- fetch gradeId
        opcoes <- carregarOpcoesDeMatricula
        render EditView { .. }

    action UpdateGradeAction { gradeId } = do
        exigirPermissao Gerenciar Notas
        grade <- fetch gradeId
        grade
            |> buildGrade
            |> validarUmaNotaPorMatricula
            >>= ifValid \case
                Left grade -> do
                    opcoes <- carregarOpcoesDeMatricula
                    render EditView { .. }
                Right grade -> do
                    grade |> updateRecord
                    setSuccessMessage "Notas atualizadas"
                    redirectTo GradesAction

    action DeleteGradeAction { gradeId } = do
        exigirPermissao Gerenciar Notas
        grade <- fetch gradeId
        deleteRecord grade
        setSuccessMessage "Notas excluídas"
        redirectTo GradesAction

-- Cada nota é validada pela regra pura do domínio (validarNota: entre 0 e 10).
buildGrade grade = grade
    |> fill @'["enrollmentId", "pe1", "avf1", "pe2", "avf2", "pe3", "avf3"]
    |> validateField #pe1 (viaDominio (validarNota "pe1"))
    |> validateField #avf1 (viaDominio (validarNota "avf1"))
    |> validateField #pe2 (viaDominio (validarNota "pe2"))
    |> validateField #avf2 (viaDominio (validarNota "avf2"))
    |> validateField #pe3 (maybe Success (viaDominio (validarNota "pe3")))
    |> validateField #avf3 (maybe Success (viaDominio (validarNota "avf3")))
    |> av3Completa

-- A AV3 só existe com PE3 e AVf3 juntas (ver notaAV3 no domínio).
av3Completa grade
    | isJust grade.pe3 /= isJust grade.avf3 =
        grade |> attachFailure #avf3 "Informe a PE3 e a AVf3 juntas (ou deixe as duas em branco)"
    | otherwise = grade

-- O boletim considera uma linha de notas por matrícula.
validarUmaNotaPorMatricula grade = do
    existente <- query @Grade
        |> filterWhere (#enrollmentId, grade.enrollmentId)
        |> fetchOneOrNothing
    pure case existente of
        Just outra | outra.id /= grade.id ->
            grade |> attachFailure #enrollmentId "Esta matrícula já tem notas lançadas. Edite o lançamento existente."
        _ -> grade

-- Opções do select de matrícula: "Aluno — Curso (status)".
-- (A assinatura é obrigatória: sem argumentos, o GHC não infere o ?modelContext.)
carregarOpcoesDeMatricula :: (?modelContext :: ModelContext) => IO [OpcaoMatricula]
carregarOpcoesDeMatricula = do
    enrollments <- query @Enrollment |> fetch
    students    <- query @Student |> fetch
    courses     <- query @Course |> fetch
    pure (opcoesDeMatricula students courses enrollments)
