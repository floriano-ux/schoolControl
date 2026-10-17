module Web.Controller.Enrollments where

import Web.Controller.Prelude
import Web.View.Enrollments.Index
import Web.View.Enrollments.New
import Web.View.Enrollments.Edit

instance Controller EnrollmentsController where
    beforeAction = exigirPermissao Visualizar Matriculas

    action EnrollmentsAction = do
        enrollments <- query @Enrollment |> fetch
        students    <- query @Student |> orderBy #name |> fetch
        courses     <- query @Course |> orderBy #name |> fetch
        render IndexView { .. }

    action NewEnrollmentAction = do
        exigirPermissao Gerenciar Matriculas
        let enrollment = newRecord @Enrollment
        students <- query @Student |> orderBy #name |> fetch
        courses  <- query @Course |> orderBy #name |> fetch
        render NewView { .. }

    action CreateEnrollmentAction = do
        exigirPermissao Gerenciar Matriculas
        let enrollment = newRecord @Enrollment
        enrollment
            |> fill @'["studentId", "courseId", "status"]
            |> validarMatriculaUnica
            >>= ifValid \case
                Left enrollment -> do
                    students <- query @Student |> orderBy #name |> fetch
                    courses  <- query @Course |> orderBy #name |> fetch
                    render NewView { .. }
                Right enrollment -> do
                    enrollment |> createRecord
                    setSuccessMessage "Matrícula criada"
                    redirectTo EnrollmentsAction

    action EditEnrollmentAction { enrollmentId } = do
        exigirPermissao Gerenciar Matriculas
        enrollment <- fetch enrollmentId
        student    <- fetch enrollment.studentId
        course     <- fetch enrollment.courseId
        render EditView { .. }

    -- Na edição só o status muda (aluno e curso definem a matrícula).
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

-- Espelha a restrição UNIQUE (student_id, course_id) do banco.
validarMatriculaUnica enrollment = do
    existente <- query @Enrollment
        |> filterWhere (#studentId, enrollment.studentId)
        |> filterWhere (#courseId, enrollment.courseId)
        |> fetchOneOrNothing
    pure case existente of
        Just _  -> enrollment |> attachFailure #courseId "Este aluno já está matriculado neste curso"
        Nothing -> enrollment
