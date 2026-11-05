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
                -- A gravação roda no monad AppM: se outra requisição criar a mesma
                -- matrícula entre a validação e o INSERT, a UNIQUE (student_id, course_id)
                -- do banco dispara e 'capturarInfra' traduz para 'Conflito' (sem erro 500).
                resultado <- executarApp (capturarInfra "matricula" (createRecord matriculaValida))
                case resultado of
                    Right _ -> do
                        setSuccessMessage "Matrícula criada"
                        redirectTo EnrollmentsAction
                    Left erro -> do
                        let enrollmentComErro = enrollment |> attachFailure #courseId (mensagemAppError erro)
                        render NewView { enrollment = enrollmentComErro, students, courses }
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
        resultado <- executarApp do
            capturarInfra "matricula/atualizar" (enrollment |> fill @'["status"] |> updateRecord)
        responderComMensagem resultado "Matrícula atualizada"

    action DeleteEnrollmentAction { enrollmentId } = do
        exigirPermissao Gerenciar Matriculas
        -- Fluxo monádico: buscar (pode não existir) -> excluir (pode falhar no banco).
        resultado <- executarApp do
            talvez <- capturarInfra "matricula/excluir" (fetchOneOrNothing enrollmentId)
            matricula <- exigirJust (NaoEncontrado "Matrícula") talvez
            capturarInfra "matricula/excluir" (deleteRecord matricula)
        responderComMensagem resultado "Matrícula excluída"

-- Converte o resultado de um fluxo AppM em mensagem flash + redirecionamento.
responderComMensagem resultado mensagemDeSucesso = do
    case resultado of
        Right _   -> setSuccessMessage mensagemDeSucesso
        Left erro -> setErrorMessage (mensagemAppError erro)
    redirectTo EnrollmentsAction
