module Web.Controller.Enrollments where

import Web.Controller.Prelude
import Web.View.Enrollments.Index
import Web.View.Enrollments.New
import Web.View.Enrollments.Show
import Web.View.Enrollments.Edit

instance Controller EnrollmentsController where
    beforeAction = exigirPermissao Visualizar Matriculas

    action EnrollmentsAction = do
        matriculas <- carregarMatriculasDetalhadas
        render IndexView { .. }

    action ShowEnrollmentAction { enrollmentId } = do
        detalhe <- buscarMatriculaDetalhada enrollmentId
        render ShowView { .. }

    action NewEnrollmentAction = do
        exigirPermissao Gerenciar Matriculas
        let enrollment = newRecord @Enrollment
        students    <- buscarAlunos
        disciplinas <- buscarDisciplinasComCurso
        render NewView { .. }

    action CreateEnrollmentAction = do
        exigirPermissao Gerenciar Matriculas
        users       <- query @User |> fetch
        subjects    <- query @Subject |> fetch
        existentes  <- query @Enrollment |> fetch
        students    <- buscarAlunos
        disciplinas <- buscarDisciplinasComCurso

        let enrollment = newRecord @Enrollment
                |> fill @'["studentId", "subjectId", "status"]

        pipelineMatriculaCPS enrollment users subjects existentes
            (\erro -> do
                let campo = case erro of
                        AlunoNaoEncontrado      -> attachFailure #studentId
                        UsuarioNaoEhAluno       -> attachFailure #studentId
                        DisciplinaNaoEncontrada -> attachFailure #subjectId
                        MatriculaDuplicada      -> attachFailure #subjectId
                render NewView { enrollment = enrollment |> campo (mensagemErroMatricula erro), students, disciplinas }
            )
            (\matriculaValida -> do
                matriculaValida |> createRecord
                setSuccessMessage "Matrícula criada"
                redirectTo EnrollmentsAction
            )

    action EditEnrollmentAction { enrollmentId } = do
        exigirPermissao Gerenciar Matriculas
        detalhe <- buscarMatriculaDetalhada enrollmentId
        render EditView { .. }

    -- Na edição só o status muda (aluno e disciplina definem a matrícula).
    action UpdateEnrollmentAction { enrollmentId } = do
        exigirPermissao Gerenciar Matriculas
        enrollment <- fetch enrollmentId
        enrollment
            |> fill @'["status"]
            |> updateRecord
        setSuccessMessage "Matrícula atualizada"
        redirectTo EnrollmentsAction

    action DeleteEnrollmentAction { enrollmentId } = do
        exigirPermissao Gerenciar Matriculas
        enrollment <- fetch enrollmentId
        deleteRecord enrollment
        setSuccessMessage "Matrícula excluída"
        redirectTo EnrollmentsAction

buscarAlunos :: (?modelContext :: ModelContext) => IO [User]
buscarAlunos = query @User |> filterWhere (#userRole, Aluno) |> orderBy #name |> fetch

buscarDisciplinasComCurso :: (?modelContext :: ModelContext) => IO [(Subject, Course)]
buscarDisciplinasComCurso = do
    subjects <- query @Subject |> orderBy #name |> fetch
    courses  <- query @Course |> fetch
    pure (sortOn rotuloDaDisciplina (disciplinasComCurso courses subjects))
