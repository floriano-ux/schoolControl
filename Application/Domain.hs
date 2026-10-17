module Application.Domain
    (
      Pesos (..)
    , Situacao (..)
    , Conceito (..)
    , ErroValidacao (..)
    , mediaPonderada
    , pesosPadrao
    , comporAV
    , mediaSimples
    , notaAV1
    , notaAV2
    , notaAV3
    , mediaParcial
    , mediaFinal
    , notaMinimaParcial
    , notaMinimaAV3
    , notaMinimaFinal
    , classificarMedia
    , situacaoDaMatricula
    , validarNota
    , validarCargaHoraria
    , validarStudent
    , validarCourse
    , validarGrade
    , mensagemDeErro
    , boletim
    , nomesAprovados
    , matriculasAtivasPorCurso
    , mediaDaTurma
      -- Relatórios (map, filter, fold, concatMap)
    , mediaPorCurso
    , rankingDeAlunos
    , alunosPorCurso
    , contarPorSituacao
    , cargaHorariaTotal
    , nomeDoAluno
    , nomeDoCurso
      -- Papéis e permissões
    , Recurso (..)
    , Operacao (..)
    , podeAcessar
    , recursosVisiveis
    , nomeDoPapel
    , nomeDaSituacao
    , nomeDoStatus
    ) where

import IHP.Prelude
import Generated.Types
import qualified Data.Text as Text
import Data.Ord (Down (..))


-- Pesos da composição de uma AV: Prova Escrita (PE) e Avaliação Formativa (AVf).
data Pesos = Pesos
    { pesoProva     :: Double
    , pesoFormativa :: Double
    } deriving (Eq, Show)

-- Situação acadêmica de um aluno numa disciplina.
data Situacao
    = Aprovado
    | Reprovado
    | AguardandoAV3   -- média de AV1 e AV2 >= 4,0, mas AV3 ainda não foi lançada
    | Trancado
    deriving (Eq, Show)

-- Faixas de desempenho
data Conceito
    = Insuficiente
    | Regular
    | Bom
    | Otimo
    | Excelente
    deriving (Eq, Show, Ord, Enum, Bounded)

-- Tipo de erro de validação
data ErroValidacao
    = CampoVazio Text
    | EmailInvalido Text
    | NotaForaDoIntervalo Text Double
    | CargaHorariaInvalida Int
    deriving (Eq, Show)


-- Regras da disciplina
-- Média entre AV1 e AV2 abaixo disso: reprovação por nota.
notaMinimaParcial :: Double
notaMinimaParcial = 4.0

-- AV3 abaixo disso: reprovação por nota.
notaMinimaAV3 :: Double
notaMinimaAV3 = 4.0

-- Nota final (NF) a partir da qual o aluno é aprovado.
notaMinimaFinal :: Double
notaMinimaFinal = 5.0


-- Currying e closures

mediaPonderada :: Pesos -> Double -> Double -> Double
mediaPonderada Pesos { pesoProva, pesoFormativa } = \pe avf ->
    arredondar ((pe * pesoProva + avf * pesoFormativa) / (pesoProva + pesoFormativa))

-- PE vale 70%, AVf vale 30%.
pesosPadrao :: Pesos
pesosPadrao = Pesos { pesoProva = 0.7, pesoFormativa = 0.3 }

-- AVn = 70% PEn + 30% AVfn
comporAV :: Double -> Double -> Double
comporAV = mediaPonderada pesosPadrao

-- Média aritmética de dois valores (pesos iguais).
mediaSimples :: Double -> Double -> Double
mediaSimples = mediaPonderada (Pesos 1 1)

-- Arredonda para 2 casas decimais, evitando que erros de ponto flutuante
arredondar :: Double -> Double
arredondar x = fromIntegral (round (x * 100) :: Integer) / 100


-- Composição das AVs e da nota final
notaAV1 :: Grade -> Double
notaAV1 grade = comporAV grade.pe1 grade.avf1

notaAV2 :: Grade -> Double
notaAV2 grade = comporAV grade.pe2 grade.avf2

-- AV3 só existe depois que PE3 e AVf3 forem lançadas.
notaAV3 :: Grade -> Maybe Double
notaAV3 grade = comporAV <$> grade.pe3 <*> grade.avf3

-- (AV1 + AV2) / 2
mediaParcial :: Grade -> Double
mediaParcial grade = mediaSimples (notaAV1 grade) (notaAV2 grade)

-- NF = ((AV1 + AV2) / 2 + AV3) / 2.
-- Enquanto a AV3 não é lançada, devolve a média parcial.
mediaFinal :: Grade -> Double
mediaFinal grade = case notaAV3 grade of
    Nothing  -> mediaParcial grade
    Just av3 -> mediaSimples (mediaParcial grade) av3


-- Classificação
--   1. (AV1 + AV2) / 2 < 4,0            -> Reprovado
--   2. AV3 ainda não lançada            -> AguardandoAV3
--   3. AV3 < 4,0                        -> Reprovado
--   4. NF >= 5,0                        -> Aprovado
--   5. NF < 5,0                         -> Reprovado

classificarMedia :: Double -> Conceito
classificarMedia media
    | media < 4.0 = Insuficiente
    | media < 6.0 = Regular
    | media < 7.5 = Bom
    | media < 9.0 = Otimo
    | otherwise   = Excelente


situacaoDaMatricula :: EnrollmentStatus -> Grade -> Situacao
situacaoDaMatricula Trancada _ = Trancado
situacaoDaMatricula _ grade
    | mediaParcial grade < notaMinimaParcial = Reprovado
    | otherwise = case notaAV3 grade of
        Nothing -> AguardandoAV3
        Just av3
            | av3 < notaMinimaAV3                  -> Reprovado
            | mediaFinal grade >= notaMinimaFinal -> Aprovado
            | otherwise                            -> Reprovado


-- ---------------------------------------------------------------------------
-- Validações (funções puras que devolvem Either ou lista de erros)
-- ---------------------------------------------------------------------------

-- Uma nota é válida se estiver entre 0 e 10.
validarNota :: Text -> Double -> Either ErroValidacao Double
validarNota campo nota
    | nota >= 0 && nota <= 10 = Right nota
    | otherwise               = Left (NotaForaDoIntervalo campo nota)

validarStudent :: Student -> [ErroValidacao]
validarStudent student =
    [ CampoVazio "name" | Text.null (Text.strip student.name) ]
    <> [ EmailInvalido student.email | not (emailValido student.email) ]

-- A carga horária de um curso precisa ser positiva.
validarCargaHoraria :: Int -> Either ErroValidacao Int
validarCargaHoraria horas
    | horas > 0 = Right horas
    | otherwise = Left (CargaHorariaInvalida horas)

validarCourse :: Course -> [ErroValidacao]
validarCourse course =
    [ CampoVazio "name" | Text.null (Text.strip course.name) ]
    <> lefts [ validarCargaHoraria course.workload ]

-- Valida todas as notas parciais de uma vez
validarGrade :: Grade -> [ErroValidacao]
validarGrade grade = lefts
    ( [ validarNota "pe1"  grade.pe1
      , validarNota "avf1" grade.avf1
      , validarNota "pe2"  grade.pe2
      , validarNota "avf2" grade.avf2
      ]
    <> opcional "pe3"  grade.pe3
    <> opcional "avf3" grade.avf3
    )
  where
    opcional campo = maybe [] (\nota -> [validarNota campo nota])

-- Um e-mail precisa de exatamente um "@", com algo antes e um domínio com ponto depois.
emailValido :: Text -> Bool
emailValido email = case Text.splitOn "@" email of
    [usuario, dominio] -> not (Text.null usuario) && "." `Text.isInfixOf` dominio
    _                  -> False

-- Mensagem amigável (em português) para mostrar no formulário.
mensagemDeErro :: ErroValidacao -> Text
mensagemDeErro erro = case erro of
    CampoVazio _                -> "Este campo é obrigatório"
    EmailInvalido _             -> "Informe um e-mail válido"
    NotaForaDoIntervalo _ _     -> "A nota deve estar entre 0 e 10"
    CargaHorariaInvalida _      -> "A carga horária deve ser maior que zero"


-- Coleções: list comprehensions, pattern matching e recursão em cauda

boletim :: [Student] -> [Enrollment] -> [Grade] -> [(Text, Double, Situacao)]
boletim students enrollments grades =
    [ (student.name, mediaFinal grade, situacaoDaMatricula enrollment.status grade)
    | student    <- students
    , enrollment <- enrollments
    , enrollment.studentId == student.id
    , grade      <- grades
    , grade.enrollmentId == enrollment.id
    ]

nomesAprovados :: [(Text, Double, Situacao)] -> [Text]
nomesAprovados linhas = [ nome | (nome, _, Aprovado) <- linhas ]

matriculasAtivasPorCurso :: [Course] -> [Enrollment] -> [(Text, Int)]
matriculasAtivasPorCurso courses enrollments =
    [ (course.name, length ativas)
    | course <- courses
    , let ativas = [ e | e@Enrollment { status = Ativa } <- enrollments
                       , e.courseId == course.id ]
    ]

mediaDaTurma :: [Double] -> Maybe Double
mediaDaTurma = go 0 0
  where
    go :: Double -> Int -> [Double] -> Maybe Double
    go _ 0 [] = Nothing
    go !soma !qtd [] = Just (soma / fromIntegral qtd)
    go !soma !qtd (x:xs) = go (soma + x) (qtd + 1) xs


-- ---------------------------------------------------------------------------
-- Relatórios: funções de alta ordem (map, filter, foldr/foldl', concatMap)
-- ---------------------------------------------------------------------------

-- Notas lançadas para as matrículas de um curso.
-- filter seleciona as matrículas do curso; concatMap junta as notas de cada uma.
notasDoCurso :: [Enrollment] -> [Grade] -> Course -> [Grade]
notasDoCurso enrollments grades course =
    concatMap notasDaMatricula (filter (\e -> e.courseId == course.id) enrollments)
  where
    notasDaMatricula enrollment = filter (\g -> g.enrollmentId == enrollment.id) grades

-- Média das notas finais (NF) de cada curso.
-- map percorre os cursos; foldr acumula (soma, quantidade) numa única passada.
mediaPorCurso :: [Course] -> [Enrollment] -> [Grade] -> [(Text, Maybe Double)]
mediaPorCurso courses enrollments grades = map mediaDoCurso courses
  where
    mediaDoCurso course =
        let (soma, qtd) = foldr somar (0, 0) (notasDoCurso enrollments grades course)
        in (course.name, if qtd == 0 then Nothing else Just (arredondar (soma / fromIntegral qtd)))

    somar :: Grade -> (Double, Int) -> (Double, Int)
    somar grade (soma, qtd) = (soma + mediaFinal grade, qtd + 1)

-- Ranking dos alunos pela média geral (média das NFs de todas as matrículas com nota).
-- Alunos sem nenhuma nota ficam de fora (mapMaybe). Empates são ordenados pelo nome.
rankingDeAlunos :: [Student] -> [Enrollment] -> [Grade] -> [(Int, Text, Double)]
rankingDeAlunos students enrollments grades =
    zipWith (\posicao (nome, media) -> (posicao, nome, media)) [1 ..]
        (sortOn (\(nome, media) -> (Down media, nome)) (mapMaybe mediaDoAluno students))
  where
    mediaDoAluno student =
        let matriculas = filter (\e -> e.studentId == student.id) enrollments
            notas      = concatMap (\e -> filter (\g -> g.enrollmentId == e.id) grades) matriculas
        in fmap (\media -> (student.name, arredondar media)) (mediaDaTurma (map mediaFinal notas))

-- Alunos de cada curso (matrículas trancadas não entram), em ordem alfabética.
-- concatMap transforma cada matrícula em zero ou um nome de aluno.
alunosPorCurso :: [Course] -> [Student] -> [Enrollment] -> [(Text, [Text])]
alunosPorCurso courses students enrollments =
    map (\course -> (course.name, nomesNoCurso course)) courses
  where
    nomesNoCurso course =
        sort (concatMap nomeDoAluno (filter (doCurso course) enrollments))

    doCurso course enrollment = enrollment.courseId == course.id && enrollment.status /= Trancada

    nomeDoAluno enrollment =
        map (.name) (filter (\s -> s.id == enrollment.studentId) students)

-- Quantos alunos estão em cada situação do boletim (foldl' estrito).
contarPorSituacao :: [(Text, Double, Situacao)] -> [(Situacao, Int)]
contarPorSituacao linhas =
    map (\situacao -> (situacao, foldl' (contar situacao) 0 linhas))
        [Aprovado, AguardandoAV3, Reprovado, Trancado]
  where
    contar situacao total (_, _, s)
        | s == situacao = total + 1
        | otherwise     = total

-- Soma da carga horária de uma lista de cursos.
cargaHorariaTotal :: [Course] -> Int
cargaHorariaTotal = foldl' (\total course -> total + course.workload) 0

-- "JOIN" em memória: nome do aluno/curso a partir do id (usado nas listagens).
nomeDoAluno :: [Student] -> Id Student -> Text
nomeDoAluno students studentId =
    maybe "(aluno removido)" (.name) (find (\s -> s.id == studentId) students)

nomeDoCurso :: [Course] -> Id Course -> Text
nomeDoCurso courses courseId =
    maybe "(curso removido)" (.name) (find (\c -> c.id == courseId) courses)


-- ---------------------------------------------------------------------------
-- Papéis e permissões
-- O papel do usuário (UserRole) vem do ENUM user_role do Postgres.
-- ---------------------------------------------------------------------------

-- Áreas do sistema que podem ser protegidas.
data Recurso
    = Estudantes
    | Cursos
    | Matriculas
    | Notas
    | Relatorios
    | Usuarios
    deriving (Eq, Show, Enum, Bounded)

-- Visualizar = listar/ver detalhes; Gerenciar = criar, editar e excluir.
data Operacao
    = Visualizar
    | Gerenciar
    deriving (Eq, Show, Enum, Bounded)

-- Tabela de permissões (pattern matching sobre o papel):
--
--   Recurso      | Admin | Coordenador | Professor | Aluno
--   -------------+-------+-------------+-----------+-------
--   Estudantes   |   G   |      G      |     V     |   -
--   Cursos       |   G   |      G      |     V     |   V
--   Matrículas   |   G   |      G      |     V     |   -
--   Notas        |   G   |      V      |     G     |   -
--   Relatórios   |   V   |      V      |     V     |   V
--   Usuários     |   G   |      -      |     -     |   -
--
-- (G = gerenciar, que inclui visualizar; V = só visualizar)
podeAcessar :: UserRole -> Operacao -> Recurso -> Bool
podeAcessar Admin _ _ = True
podeAcessar Coordenador operacao recurso = case recurso of
    Usuarios -> False
    Notas    -> operacao == Visualizar
    _        -> True
podeAcessar Professor operacao recurso = case recurso of
    Usuarios -> False
    Notas    -> True
    _        -> operacao == Visualizar
podeAcessar Aluno operacao recurso =
    operacao == Visualizar && recurso `elem` [Cursos, Relatorios]

-- Recursos que aparecem no menu para um papel (aplicação parcial + filter).
recursosVisiveis :: UserRole -> [Recurso]
recursosVisiveis papel = filter (podeAcessar papel Visualizar) [minBound .. maxBound]

nomeDoPapel :: UserRole -> Text
nomeDoPapel papel = case papel of
    Admin       -> "Administrador"
    Coordenador -> "Coordenador"
    Professor   -> "Professor"
    Aluno       -> "Aluno"

nomeDaSituacao :: Situacao -> Text
nomeDaSituacao situacao = case situacao of
    Aprovado      -> "Aprovado"
    Reprovado     -> "Reprovado"
    AguardandoAV3 -> "Aguardando AV3"
    Trancado      -> "Trancado"

nomeDoStatus :: EnrollmentStatus -> Text
nomeDoStatus status = case status of
    Ativa     -> "Ativa"
    Trancada  -> "Trancada"
    Concluida -> "Concluída"