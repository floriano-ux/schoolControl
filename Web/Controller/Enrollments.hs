module Web.Controller.Enrollments where

import Web.Controller.Prelude
import Web.View.Enrollments.Index
import Web.View.Enrollments.New
import Web.View.Enrollments.Show
import Web.View.Enrollments.Edit

instance Controller EnrollmentsController where
    beforeAction = exigirPermissao Visualizar Matriculas

    action EnrollmentsAction = do
        enrollments <- query @Enrollment |> fetch
        students    <- query @Student |> orderBy #name |> fetch
        courses     <- query @Course |> orderBy #name |> fetch
        render IndexView { .. }

    action ShowEnrollmentAction { enrollmentId } = do
        enrollment <- fetch enrollmentId
        student    <- fetch enrollment.studentId
        course     <- fetch enrollment.courseId
        render ShowView { .. }

    action NewEnrollmentAction = do
        exigirPermissao Gerenciar Matriculas
        let enrollment = newRecord @Enrollment
        students <- query @Student |> orderBy #name |> fetch
        courses  <- query @Course |> orderBy #name |> fetch
        render NewView { .. }

    action CreateEnrollmentAction = do
        exigirPermissao Gerenciar Matriculas
        students            <- query @Student |> orderBy #name |> fetch
        courses             <- query @Course |> orderBy #name |> fetch
        existingEnrollments <- query @Enrollment |> fetch

        let enrollment = newRecord @Enrollment
                |> fill @'["studentId", "courseId", "status"]

        -- Encadeia a validação em CPS (aluno -> curso -> duplicidade).
        pipelineMatriculaCPS enrollment students courses existingEnrollments
            (\erro -> do
                let enrollmentComErro = case erro of
                        AlunoNaoEncontrado -> enrollment |> attachFailure #studentId (mensagemErroMatricula erro)
                        CursoNaoEncontrado -> enrollment |> attachFailure #courseId (mensagemErroMatricula erro)
                        MatriculaDuplicada -> enrollment |> attachFailure #courseId (mensagemErroMatricula erro)
                render NewView { enrollment = enrollmentComErro, students, courses }
            )
            (\matriculaValida -> do
                matriculaValida |> createRecord
                setSuccessMessage "Matrícula criada"
                redirectTo EnrollmentsAction
            )

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
