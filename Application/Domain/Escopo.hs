{-# LANGUAGE NoFieldSelectors #-}
module Application.Domain.Escopo
    ( ministra
    , disciplinasDoProfessor
    , disciplinasVisiveis
    , matriculasVisiveis
    , matriculasDaDisciplina
    , semNotaLancada
    , professorValido
    , nomeDoProfessor
    ) where

import IHP.Prelude
import Generated.Types
import Application.Domain.Matricula (MatriculaDetalhada (..))

ministra :: User -> Subject -> Bool
ministra user subject = user.userRole == Professor && subject.professorId == Just user.id

disciplinasDoProfessor :: User -> [Subject] -> [Subject]
disciplinasDoProfessor user = filter (ministra user)

-- O professor só enxerga o que é das disciplinas que ministra; o aluno, só o que é dele.
disciplinasVisiveis :: User -> [Subject] -> [Subject]
disciplinasVisiveis user subjects = case user.userRole of
    Professor -> disciplinasDoProfessor user subjects
    _         -> subjects

matriculasVisiveis :: User -> [MatriculaDetalhada] -> [MatriculaDetalhada]
matriculasVisiveis user matriculas = case user.userRole of
    Professor -> filter (\m -> ministra user m.disciplina) matriculas
    Aluno     -> filter (\m -> m.aluno.id == user.id) matriculas
    _         -> matriculas

matriculasDaDisciplina :: Id Subject -> [MatriculaDetalhada] -> [MatriculaDetalhada]
matriculasDaDisciplina subjectId =
    sortOn (\m -> m.aluno.name) . filter (\m -> m.disciplina.id == subjectId)

-- Matrículas que ainda não têm lançamento de notas.
semNotaLancada :: [Grade] -> [MatriculaDetalhada] -> [MatriculaDetalhada]
semNotaLancada grades = filter (\m -> not (any (\g -> g.enrollmentId == m.matricula.id) grades))

-- Disciplina sem professor é permitida; se houver, precisa ter o papel de professor.
professorValido :: [User] -> Maybe (Id User) -> Bool
professorValido _     Nothing            = True
professorValido users (Just professorId) =
    any (\u -> u.id == professorId && u.userRole == Professor) users

nomeDoProfessor :: [User] -> Subject -> Text
nomeDoProfessor users subject = case subject.professorId of
    Nothing          -> "Sem professor"
    Just professorId -> maybe "(professor removido)" (.name) (find (\u -> u.id == professorId) users)
