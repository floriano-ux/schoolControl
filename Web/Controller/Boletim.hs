module Web.Controller.Boletim where

import Web.Controller.Prelude
import Web.View.Boletim.Index

instance Controller BoletimController where
    beforeAction = exigirPermissao Visualizar Boletim

    action MeuBoletimAction = autoRefresh do
        let aluno = currentUser
        enrollments <- query @Enrollment |> filterWhere (#studentId, aluno.id) |> fetch
        subjects    <- query @Subject |> filterWhereIn (#id, map (.subjectId) enrollments) |> fetch
        courses     <- query @Course |> filterWhereIn (#id, map (.courseId) subjects) |> fetch
        grades      <- query @Grade |> filterWhereIn (#enrollmentId, map (.id) enrollments) |> fetch
        let linhas = boletimDoAluno (detalharMatriculas [aluno] subjects courses enrollments) grades
        render IndexView { .. }
