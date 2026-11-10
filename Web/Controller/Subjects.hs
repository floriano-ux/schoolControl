module Web.Controller.Subjects where

import Web.Controller.Prelude
import Web.View.Subjects.Index
import Web.View.Subjects.New
import Web.View.Subjects.Edit
import Web.View.Subjects.Show

instance Controller SubjectsController where
    beforeAction = exigirPermissao Visualizar Disciplinas

    action SubjectsAction = do
        subjects <- query @Subject |> orderBy #name |> fetch
        courses  <- query @Course |> orderBy #name |> fetch
        render IndexView { .. }

    action NewSubjectAction = do
        exigirPermissao Gerenciar Disciplinas
        let subject = newRecord @Subject
        courses <- query @Course |> orderBy #name |> fetch
        render NewView { .. }

    action ShowSubjectAction { subjectId } = do
        subject <- fetch subjectId
        course  <- fetch subject.courseId
        render ShowView { .. }

    action EditSubjectAction { subjectId } = do
        exigirPermissao Gerenciar Disciplinas
        subject <- fetch subjectId
        courses <- query @Course |> orderBy #name |> fetch
        render EditView { .. }

    action UpdateSubjectAction { subjectId } = do
        exigirPermissao Gerenciar Disciplinas
        subject <- fetch subjectId
        courses <- query @Course |> orderBy #name |> fetch
        subject
            |> buildSubject
            |> validarNomeUnicoNoCurso
            >>= ifValid \case
                Left subject -> render EditView { .. }
                Right subject -> do
                    subject <- subject |> updateRecord
                    setSuccessMessage "Disciplina atualizada"
                    redirectTo EditSubjectAction { .. }

    action CreateSubjectAction = do
        exigirPermissao Gerenciar Disciplinas
        courses <- query @Course |> orderBy #name |> fetch
        newRecord @Subject
            |> buildSubject
            |> validarNomeUnicoNoCurso
            >>= ifValid \case
                Left subject -> render NewView { .. }
                Right subject -> do
                    subject |> createRecord
                    setSuccessMessage "Disciplina criada"
                    redirectTo SubjectsAction

    action DeleteSubjectAction { subjectId } = do
        exigirPermissao Gerenciar Disciplinas
        subject <- fetch subjectId
        deleteRecord subject
        setSuccessMessage "Disciplina excluída"
        redirectTo SubjectsAction

buildSubject subject = subject
    |> fill @'["name", "workload", "courseId"]
    |> validateField #name (nonEmpty |> withCustomErrorMessage "Informe o nome da disciplina")
    |> validateField #workload (viaDominio validarCargaHoraria)

-- Espelha a restrição UNIQUE (course_id, name) do banco.
validarNomeUnicoNoCurso subject = do
    existente <- query @Subject
        |> filterWhere (#courseId, subject.courseId)
        |> filterWhereCaseInsensitive (#name, subject.name)
        |> fetchOneOrNothing
    pure case existente of
        Just outra | outra.id /= subject.id ->
            subject |> attachFailure #name "Este curso já tem uma disciplina com esse nome"
        _ -> subject
