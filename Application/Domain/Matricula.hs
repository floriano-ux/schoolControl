{-# LANGUAGE NoFieldSelectors #-}
module Application.Domain.Matricula
    ( MatriculaDetalhada (..)
    , detalharMatriculas
    , disciplinasComCurso
    , rotuloDaDisciplina
    , nomeDoAluno
    , nomeDoCurso
    , nomeDaDisciplina
    , nomeDoStatus
    , ErroMatricula (..)
    , mensagemErroMatricula
    , pipelineMatriculaCPS
    ) where

import IHP.Prelude
import Generated.Types
import Application.Domain.Permissoes (ehAluno)

-- Read model: a matrícula já cruzada com aluno, disciplina e curso.
data MatriculaDetalhada = MatriculaDetalhada
    { matricula  :: Enrollment
    , aluno      :: User
    , disciplina :: Subject
    , curso      :: Course
    } deriving (Eq, Show)

-- "JOIN" em memória. Matrículas sem aluno, disciplina ou curso correspondente ficam de fora.
detalharMatriculas :: [User] -> [Subject] -> [Course] -> [Enrollment] -> [MatriculaDetalhada]
detalharMatriculas us ss cs es =
    [ MatriculaDetalhada e u s c
    | e <- es, u <- us, u.id == e.studentId
    , s <- ss, s.id == e.subjectId
    , c <- cs, c.id == s.courseId ]

disciplinasComCurso :: [Course] -> [Subject] -> [(Subject, Course)]
disciplinasComCurso cs ss = [ (s, c) | s <- ss, c <- cs, c.id == s.courseId ]

-- "Disciplina (Curso)"
rotuloDaDisciplina :: (Subject, Course) -> Text
rotuloDaDisciplina (s, c) = s.name <> " (" <> c.name <> ")"

nomeDoAluno :: [User] -> Id User -> Text
nomeDoAluno users userId =
    maybe "(aluno removido)" (.name) (find (\u -> u.id == userId) users)

nomeDoCurso :: [Course] -> Id Course -> Text
nomeDoCurso courses courseId =
    maybe "(curso removido)" (.name) (find (\c -> c.id == courseId) courses)

nomeDaDisciplina :: [Subject] -> Id Subject -> Text
nomeDaDisciplina subjects subjectId =
    maybe "(disciplina removida)" (.name) (find (\s -> s.id == subjectId) subjects)

nomeDoStatus :: EnrollmentStatus -> Text
nomeDoStatus status = case status of
    Ativa     -> "Ativa"
    Trancada  -> "Trancada"
    Concluida -> "Concluída"


-- ---------------------------------------------------------------------------
-- Pipeline de matrícula em Continuation Passing Style (CPS)
--
-- Cada etapa recebe a continuação de erro ('onErro') e a do próximo passo ('k'):
--   1. O aluno selecionado existe
--   2. O usuário selecionado tem papel de aluno
--   3. A disciplina selecionada existe
--   4. O aluno ainda não está matriculado na disciplina
-- ---------------------------------------------------------------------------

data ErroMatricula
    = AlunoNaoEncontrado
    | UsuarioNaoEhAluno
    | DisciplinaNaoEncontrada
    | MatriculaDuplicada
    deriving (Eq, Show)

mensagemErroMatricula :: ErroMatricula -> Text
mensagemErroMatricula erro = case erro of
    AlunoNaoEncontrado      -> "Selecione um aluno válido"
    UsuarioNaoEhAluno       -> "O usuário selecionado não é um aluno"
    DisciplinaNaoEncontrada -> "Selecione uma disciplina válida"
    MatriculaDuplicada      -> "Este aluno já está matriculado nesta disciplina"

pipelineMatriculaCPS
    :: Enrollment
    -> [User]
    -> [Subject]
    -> [Enrollment]
    -> (ErroMatricula -> r)
    -> (Enrollment -> r)
    -> r
pipelineMatriculaCPS enrollment users subjects existentes onErro onSucesso =
    etapa (isJust usuario) AlunoNaoEncontrado \() ->
        etapa (maybe False ehAluno usuario) UsuarioNaoEhAluno \() ->
            etapa disciplinaExiste DisciplinaNaoEncontrada \() ->
                etapa semDuplicidade MatriculaDuplicada \() ->
                    onSucesso enrollment
  where
    usuario = find (\u -> u.id == enrollment.studentId) users
    disciplinaExiste = any (\s -> s.id == enrollment.subjectId) subjects
    semDuplicidade = not (any duplicada existentes)
    duplicada e =
        e.studentId == enrollment.studentId
        && e.subjectId == enrollment.subjectId
        && e.id /= enrollment.id

    etapa True  _    k = k ()
    etapa False erro _ = onErro erro
