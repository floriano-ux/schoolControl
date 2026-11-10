{-# LANGUAGE NoFieldSelectors #-}
module Application.Domain.Escopo
    ( ministra
    , disciplinasDoProfessor
    , disciplinasVisiveis
    , cursosVisiveis
    , matriculasVisiveis
    , alunosVisiveis
    , matriculaVisivel
    , matriculasDaDisciplina
    , semNotaLancada
    , professorValido
    , nomeDoProfessor
    , alunosComDisciplinas
    , redistribuirDisciplinas
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

cursosVisiveis :: User -> [Subject] -> [Course] -> [Course]
cursosVisiveis user subjects courses = case user.userRole of
    Professor -> filter (\c -> any (\s -> s.courseId == c.id) (disciplinasDoProfessor user subjects)) courses
    _         -> courses

matriculasVisiveis :: User -> [MatriculaDetalhada] -> [MatriculaDetalhada]
matriculasVisiveis user matriculas = case user.userRole of
    Professor -> filter (\m -> ministra user m.disciplina) matriculas
    Aluno     -> filter (\m -> m.aluno.id == user.id) matriculas
    _         -> matriculas

matriculaVisivel :: User -> MatriculaDetalhada -> Bool
matriculaVisivel user m = not (null (matriculasVisiveis user [m]))

-- Professor: só os alunos matriculados nas suas disciplinas (recebe as matrículas já escopadas).
alunosVisiveis :: User -> [MatriculaDetalhada] -> [User] -> [User]
alunosVisiveis user matriculas users = case user.userRole of
    Professor -> filter (\u -> any (\m -> m.aluno.id == u.id) matriculas) users
    _         -> users

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

-- Linhas de "Meus alunos": cada aluno com as disciplinas (visíveis) em que está matriculado.
-- Com uma disciplina escolhida, ficam só os alunos matriculados nela.
alunosComDisciplinas :: Maybe (Id Subject) -> [MatriculaDetalhada] -> [User] -> [(User, [Text])]
alunosComDisciplinas disciplinaId matriculas users =
    [ (u, sort (map (\m -> m.disciplina.name) doAluno))
    | u <- users
    , let doAluno = filter (\m -> m.aluno.id == u.id) matriculas
    , maybe True (\sid -> any (\m -> m.disciplina.id == sid) doAluno) disciplinaId
    ]

-- Atribui ao professor exatamente as disciplinas escolhidas: as marcadas passam a ser dele
-- e as que eram dele e foram desmarcadas ficam sem professor. Devolve só as que mudaram.
redistribuirDisciplinas :: Id User -> [Id Subject] -> [Subject] -> [Subject]
redistribuirDisciplinas professorId escolhidas subjects =
    [ s |> set #professorId novo
    | s <- subjects
    , let novo
            | s.id `elem` escolhidas            = Just professorId
            | s.professorId == Just professorId = Nothing
            | otherwise                         = s.professorId
    , novo /= s.professorId
    ]
