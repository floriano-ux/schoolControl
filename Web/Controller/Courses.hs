module Web.Controller.Courses where

import Web.Controller.Prelude
import Web.View.Courses.Index
import Web.View.Courses.New
import Web.View.Courses.Edit
import Web.View.Courses.Show

instance Controller CoursesController where
    -- Todo mundo logado com acesso a Cursos pode ver; criar/editar/excluir exige Gerenciar.
    beforeAction = exigirPermissao Visualizar Cursos

    action CoursesAction = do
        courses <- query @Course |> orderBy #name |> fetch
        render IndexView { .. }

    action NewCourseAction = do
        exigirPermissao Gerenciar Cursos
        let course = newRecord
        render NewView { .. }

    action ShowCourseAction { courseId } = do
        course <- fetch courseId
        render ShowView { .. }

    action EditCourseAction { courseId } = do
        exigirPermissao Gerenciar Cursos
        course <- fetch courseId
        render EditView { .. }

    action UpdateCourseAction { courseId } = do
        exigirPermissao Gerenciar Cursos
        course <- fetch courseId
        course
            |> buildCourse
            |> ifValid \case
                Left course -> render EditView { .. }
                Right course -> do
                    course <- course |> updateRecord
                    setSuccessMessage "Curso atualizado"
                    redirectTo EditCourseAction { .. }

    action CreateCourseAction = do
        exigirPermissao Gerenciar Cursos
        let course = newRecord @Course
        course
            |> buildCourse
            |> ifValid \case
                Left course -> render NewView { .. } 
                Right course -> do
                    course <- course |> createRecord
                    setSuccessMessage "Curso criado"
                    redirectTo CoursesAction

    action DeleteCourseAction { courseId } = do
        exigirPermissao Gerenciar Cursos
        course <- fetch courseId
        deleteRecord course
        setSuccessMessage "Curso excluído"
        redirectTo CoursesAction

-- A carga horária é validada pela regra pura do domínio (validarCargaHoraria).
buildCourse course = course
    |> fill @'["name", "workload"]
    |> validateField #name (nonEmpty |> withCustomErrorMessage "Informe o nome do curso")
    |> validateField #workload (viaDominio validarCargaHoraria)
