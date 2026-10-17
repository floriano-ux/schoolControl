module Web.Controller.Students where

import Web.Controller.Prelude
import Web.View.Students.Index
import Web.View.Students.New
import Web.View.Students.Edit
import Web.View.Students.Show

instance Controller StudentsController where
    beforeAction = exigirPermissao Visualizar Estudantes

    action StudentsAction = do
        students <- query @Student |> orderBy #name |> fetch
        render IndexView { .. }

    action NewStudentAction = do
        exigirPermissao Gerenciar Estudantes
        let student = newRecord
        render NewView { .. }

    action ShowStudentAction { studentId } = do
        student <- fetch studentId
        render ShowView { .. }

    action EditStudentAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- fetch studentId
        render EditView { .. }

    action UpdateStudentAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- fetch studentId
        student
            |> buildStudent
            |> validarEmailUnico
            >>= ifValid \case
                Left student -> render EditView { .. }
                Right student -> do
                    student <- student |> updateRecord
                    setSuccessMessage "Estudante atualizado"
                    redirectTo EditStudentAction { .. }

    action CreateStudentAction = do
        exigirPermissao Gerenciar Estudantes
        let student = newRecord @Student
        student
            |> buildStudent
            |> validarEmailUnico
            >>= ifValid \case
                Left student -> render NewView { .. } 
                Right student -> do
                    student <- student |> createRecord
                    setSuccessMessage "Estudante criado"
                    redirectTo StudentsAction

    action DeleteStudentAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- fetch studentId
        deleteRecord student
        setSuccessMessage "Estudante excluído"
        redirectTo StudentsAction

buildStudent student = student
    |> fill @'["name", "email"]
    |> validateField #name (nonEmpty |> withCustomErrorMessage "Informe o nome do estudante")
    |> validateField #email (isEmail |> withCustomErrorMessage "Informe um e-mail válido")

-- Espelha a restrição UNIQUE (email) do banco, mostrando o erro no formulário
-- em vez de estourar uma exceção do Postgres.
validarEmailUnico student = student
    |> withCustomErrorMessageIO "Este e-mail já está cadastrado" validateIsUnique #email
