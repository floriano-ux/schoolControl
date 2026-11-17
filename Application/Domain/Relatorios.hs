module Application.Domain.Relatorios
    ( boletim
    , boletimDoAluno
    , mediaGeral
    , nomesAprovados
    , matriculasAtivasPorCurso
    , mediaPorCurso
    , mediaPorDisciplina
    , rankingDeAlunos
    , alunosPorCurso
    , contarPorSituacao
    , cargaHorariaTotal
    , FiltroRelatorio (..)
    , filtroVazio
    , filtroAtivo
    , filtrarPorCurso
    , filtrarPorDisciplina
    , filtrarPorStatus
    , filtrarCursos
    , filtrarDisciplinas
    , aplicarFiltro
    , parseStatus
    , totalAlunosFiltrados
    , totalMatriculasAtivas
    , taxaAprovacao
    , percentualAprovados
    ) where

import IHP.Prelude
import Generated.Types
import Application.Domain.Notas
import Application.Domain.Matricula
import qualified Data.Text as Text
import Data.Ord (Down (..))
import Data.List (nubBy)

-- Uma linha por nota lançada: (aluno, NF, situação).
boletim :: [MatriculaDetalhada] -> [Grade] -> [(Text, Double, Situacao)]
boletim matriculas grades =
    [ (m.aluno.name, mediaFinal grade, situacaoDaMatricula m.matricula.status grade)
    | m     <- matriculas
    , grade <- grades
    , grade.enrollmentId == m.matricula.id
    ]

-- Boletim individual: cada matrícula com a sua nota (se já lançada), por curso e disciplina.
boletimDoAluno :: [MatriculaDetalhada] -> [Grade] -> [(MatriculaDetalhada, Maybe Grade)]
boletimDoAluno matriculas grades =
    sortOn (\(m, _) -> (m.curso.name, m.disciplina.name))
        [ (m, find (\g -> g.enrollmentId == m.matricula.id) grades) | m <- matriculas ]

-- Média das NFs das matrículas com nota lançada.
mediaGeral :: [(MatriculaDetalhada, Maybe Grade)] -> Maybe Double
mediaGeral linhas = arredondar <$> mediaDaTurma [ mediaFinal g | (_, Just g) <- linhas ]

nomesAprovados :: [(Text, Double, Situacao)] -> [Text]
nomesAprovados linhas = [ nome | (nome, _, Aprovado) <- linhas ]

matriculasAtivasPorCurso :: [Course] -> [MatriculaDetalhada] -> [(Text, Int)]
matriculasAtivasPorCurso courses matriculas =
    [ (course.name, length ativas)
    | course <- courses
    , let ativas = [ m | m <- matriculas, m.curso.id == course.id, m.matricula.status == Ativa ]
    ]

-- Notas lançadas para um conjunto de matrículas (concatMap junta as notas de cada uma).
notasDe :: [Grade] -> [MatriculaDetalhada] -> [Grade]
notasDe grades = concatMap (\m -> filter (\g -> g.enrollmentId == m.matricula.id) grades)

-- foldr acumula (soma, quantidade) numa única passada.
mediaDasNotas :: [Grade] -> Maybe Double
mediaDasNotas notas =
    let (soma, qtd) = foldr somar (0, 0) notas
    in if qtd == 0 then Nothing else Just (arredondar (soma / fromIntegral qtd))
  where
    somar :: Grade -> (Double, Int) -> (Double, Int)
    somar grade (soma, qtd) = (soma + mediaFinal grade, qtd + 1)

mediaPorCurso :: [Course] -> [MatriculaDetalhada] -> [Grade] -> [(Text, Maybe Double)]
mediaPorCurso courses matriculas grades = map mediaDoCurso courses
  where
    mediaDoCurso course =
        (course.name, mediaDasNotas (notasDe grades (filter (\m -> m.curso.id == course.id) matriculas)))

mediaPorDisciplina :: [(Subject, Course)] -> [MatriculaDetalhada] -> [Grade] -> [(Text, Maybe Double)]
mediaPorDisciplina disciplinas matriculas grades = map mediaDaDisciplina disciplinas
  where
    mediaDaDisciplina par@(subject, _) =
        (rotuloDaDisciplina par, mediaDasNotas (notasDe grades (filter (\m -> m.disciplina.id == subject.id) matriculas)))

-- Ranking pela média geral (média das NFs de todas as matrículas com nota).
-- Alunos sem nenhuma nota ficam de fora; empates são ordenados pelo nome.
rankingDeAlunos :: [MatriculaDetalhada] -> [Grade] -> [(Int, Text, Double)]
rankingDeAlunos matriculas grades =
    zipWith (\posicao (nome, media) -> (posicao, nome, media)) [1 ..]
        (sortOn (\(nome, media) -> (Down media, nome)) (mapMaybe mediaDoAluno (alunosDistintos matriculas)))
  where
    mediaDoAluno aluno =
        let notas = notasDe grades (filter (\m -> m.aluno.id == aluno.id) matriculas)
        in fmap (\media -> (aluno.name, arredondar media)) (mediaDaTurma (map mediaFinal notas))

-- Alunos de cada curso (matrículas trancadas não entram), sem repetição e em ordem alfabética.
alunosPorCurso :: [Course] -> [MatriculaDetalhada] -> [(Text, [Text])]
alunosPorCurso courses matriculas =
    map (\course -> (course.name, nomesNoCurso course)) courses
  where
    nomesNoCurso course =
        sort (map (.name) (alunosDistintos (filter (doCurso course) matriculas)))
    doCurso course m = m.curso.id == course.id && m.matricula.status /= Trancada

alunosDistintos :: [MatriculaDetalhada] -> [User]
alunosDistintos = nubBy (\a b -> a.id == b.id) . map (.aluno)

-- Quantos alunos estão em cada situação do boletim (foldl' estrito).
contarPorSituacao :: [(Text, Double, Situacao)] -> [(Situacao, Int)]
contarPorSituacao linhas =
    map (\situacao -> (situacao, foldl' (contar situacao) 0 linhas))
        [Aprovado, AguardandoAV3, Reprovado, Trancado]
  where
    contar situacao total (_, _, s)
        | s == situacao = total + 1
        | otherwise     = total

cargaHorariaTotal :: [Course] -> Int
cargaHorariaTotal = foldl' (\total course -> total + course.workload) 0


-- ---------------------------------------------------------------------------
-- Filtros do Dashboard e KPIs. Nothing indica ausência de restrição no campo.
-- ---------------------------------------------------------------------------

data FiltroRelatorio = FiltroRelatorio
    { cursoId      :: Maybe (Id Course)
    , disciplinaId :: Maybe (Id Subject)
    , status       :: Maybe EnrollmentStatus
    } deriving (Eq, Show)

filtroVazio :: FiltroRelatorio
filtroVazio = FiltroRelatorio Nothing Nothing Nothing

filtroAtivo :: FiltroRelatorio -> Bool
filtroAtivo = (/= filtroVazio)

filtrarPor :: Eq b => (a -> b) -> Maybe b -> [a] -> [a]
filtrarPor _     Nothing     xs = xs
filtrarPor campo (Just alvo) xs = filter (\x -> campo x == alvo) xs

filtrarPorCurso :: Maybe (Id Course) -> [MatriculaDetalhada] -> [MatriculaDetalhada]
filtrarPorCurso = filtrarPor (\m -> m.curso.id)

filtrarPorDisciplina :: Maybe (Id Subject) -> [MatriculaDetalhada] -> [MatriculaDetalhada]
filtrarPorDisciplina = filtrarPor (\m -> m.disciplina.id)

filtrarPorStatus :: Maybe EnrollmentStatus -> [MatriculaDetalhada] -> [MatriculaDetalhada]
filtrarPorStatus = filtrarPor (\m -> m.matricula.status)

aplicarFiltro :: FiltroRelatorio -> [MatriculaDetalhada] -> [MatriculaDetalhada]
aplicarFiltro FiltroRelatorio { cursoId, disciplinaId, status } =
    filtrarPorStatus status . filtrarPorDisciplina disciplinaId . filtrarPorCurso cursoId

filtrarCursos :: Maybe (Id Course) -> [Course] -> [Course]
filtrarCursos = filtrarPor (.id)

filtrarDisciplinas :: FiltroRelatorio -> [(Subject, Course)] -> [(Subject, Course)]
filtrarDisciplinas FiltroRelatorio { cursoId, disciplinaId } =
    filtrarPor (\(s, _) -> s.id) disciplinaId . filtrarPor (\(_, c) -> c.id) cursoId

-- Converte texto em EnrollmentStatus (case-insensitive).
parseStatus :: Text -> Maybe EnrollmentStatus
parseStatus txt = case Text.toLower (Text.strip txt) of
    "ativa"     -> Just Ativa
    "trancada"  -> Just Trancada
    "concluida" -> Just Concluida
    "concluída" -> Just Concluida
    _           -> Nothing

-- Sem filtro: total de alunos cadastrados. Com filtro: alunos distintos nas matrículas filtradas.
totalAlunosFiltrados :: FiltroRelatorio -> [User] -> [MatriculaDetalhada] -> Int
totalAlunosFiltrados filtro users matriculas
    | filtroAtivo filtro = length (alunosDistintos matriculas)
    | otherwise          = length users

totalMatriculasAtivas :: [MatriculaDetalhada] -> Int
totalMatriculasAtivas = length . filter (\m -> m.matricula.status == Ativa)

-- Percentual de aprovação (0% a 100%); Nothing se não houver notas lançadas.
taxaAprovacao :: [(Text, Double, Situacao)] -> Maybe Double
taxaAprovacao []     = Nothing
taxaAprovacao linhas =
    let aprovados = length (nomesAprovados linhas)
        total     = length linhas
    in Just (arredondar (fromIntegral aprovados / fromIntegral total * 100))

percentualAprovados :: [(Text, Double, Situacao)] -> Maybe Double
percentualAprovados = taxaAprovacao
