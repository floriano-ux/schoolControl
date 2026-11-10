module Web.Controller.Grades where

import Web.Controller.Prelude
import Web.View.Grades.Index
import Web.View.Grades.New
import Web.View.Grades.Show
import Web.View.Grades.Edit

instance Controller GradesController where
    beforeAction = exigirPermissao Visualizar Notas

    action GradesAction = do
        grades     <- query @Grade |> fetch
        matriculas <- matriculasVisiveisPara currentUser
        render IndexView { .. }

    action ShowGradeAction { gradeId } = do
        grade   <- fetch gradeId
        detalhe <- buscarMatriculaDetalhada grade.enrollmentId
        exigirMatriculaVisivel currentUser detalhe
        render ShowView { .. }

    -- Etapa 1: escolher a disciplina (?subjectId). Etapa 2: escolher o aluno e lançar as notas.
    action NewGradeAction = do
        exigirPermissao Gerenciar Notas
        let grade = newRecord @Grade
        Lancamento { disciplinas, escolhida, pendentes } <- carregarLancamento currentUser (paramOrNothing "subjectId") Nothing
        render NewView { grade, disciplinas, escolhida, opcoes = opcoesDeAluno pendentes }

    action CreateGradeAction = do
        exigirPermissao Gerenciar Notas
        Lancamento { disciplinas, escolhida, pendentes } <- carregarLancamento currentUser (paramOrNothing "subjectId") Nothing
        newRecord @Grade
            |> buildGrade
            |> validarAlunoPendente pendentes
            |> validarUmaNotaPorMatricula
            >>= ifValid \case
                Left grade -> render NewView { grade, disciplinas, escolhida, opcoes = opcoesDeAluno pendentes }
                Right grade -> do
                    grade |> createRecord
                    setSuccessMessage "Notas lançadas"
                    redirectTo GradesAction

    action EditGradeAction { gradeId } = do
        exigirPermissao Gerenciar Notas
        grade   <- fetch gradeId
        detalhe <- buscarMatriculaDetalhada grade.enrollmentId
        exigirMatriculaVisivel currentUser detalhe
        Lancamento { pendentes } <- carregarLancamento currentUser (Just detalhe.disciplina.id) (Just grade.id)
        render EditView { grade, detalhe, opcoes = opcoesDeAluno pendentes }

    action UpdateGradeAction { gradeId } = do
        exigirPermissao Gerenciar Notas
        grade   <- fetch gradeId
        detalhe <- buscarMatriculaDetalhada grade.enrollmentId
        exigirMatriculaVisivel currentUser detalhe
        Lancamento { pendentes } <- carregarLancamento currentUser (Just detalhe.disciplina.id) (Just grade.id)
        grade
            |> buildGrade
            |> validarAlunoPendente pendentes
            |> validarUmaNotaPorMatricula
            >>= ifValid \case
                Left grade -> render EditView { grade, detalhe, opcoes = opcoesDeAluno pendentes }
                Right grade -> do
                    grade |> updateRecord
                    setSuccessMessage "Notas atualizadas"
                    redirectTo GradesAction

    action DeleteGradeAction { gradeId } = do
        exigirPermissao Gerenciar Notas
        grade   <- fetch gradeId
        detalhe <- buscarMatriculaDetalhada grade.enrollmentId
        exigirMatriculaVisivel currentUser detalhe
        deleteRecord grade
        setSuccessMessage "Notas excluídas"
        redirectTo GradesAction

data Lancamento = Lancamento
    { disciplinas :: [(Subject, Course)]
    , escolhida   :: Maybe (Subject, Course)
    , pendentes   :: [MatriculaDetalhada]
    }

-- Disciplinas que o usuário pode lançar e, para a escolhida, os alunos matriculados
-- ainda sem notas (no Edit, o lançamento atual não conta como "já lançado").
carregarLancamento
    :: (?modelContext :: ModelContext)
    => User -> Maybe (Id Subject) -> Maybe (Id Grade) -> IO Lancamento
carregarLancamento usuario disciplinaId gradeAtual = do
    subjects   <- query @Subject |> fetch
    courses    <- query @Course |> fetch
    grades     <- query @Grade |> fetch
    matriculas <- matriculasVisiveisPara usuario
    let disciplinas = sortOn rotuloDaDisciplina (disciplinasComCurso courses (disciplinasVisiveis usuario subjects))
        escolhida   = disciplinaId >>= \sid -> find (\(s, _) -> s.id == sid) disciplinas
        outrasNotas = filter (\g -> Just g.id /= gradeAtual) grades
        pendentes   = maybe [] (\(s, _) -> semNotaLancada outrasNotas (matriculasDaDisciplina s.id matriculas)) escolhida
    pure Lancamento { .. }

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

-- O aluno escolhido precisa estar matriculado na disciplina escolhida (e no escopo do usuário).
validarAlunoPendente :: [MatriculaDetalhada] -> Grade -> Grade
validarAlunoPendente pendentes grade
    | any (\m -> m.matricula.id == grade.enrollmentId) pendentes = grade
    | otherwise = grade |> attachFailure #enrollmentId "Selecione um aluno matriculado nesta disciplina"

-- O boletim considera uma linha de notas por matrícula.
validarUmaNotaPorMatricula grade = do
    existente <- query @Grade
        |> filterWhere (#enrollmentId, grade.enrollmentId)
        |> fetchOneOrNothing
    pure case existente of
        Just outra | outra.id /= grade.id ->
            grade |> attachFailure #enrollmentId "Esta matrícula já tem notas lançadas. Edite o lançamento existente."
        _ -> grade
