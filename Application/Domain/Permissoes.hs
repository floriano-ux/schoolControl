module Application.Domain.Permissoes
    ( Recurso (..)
    , Operacao (..)
    , podeAcessar
    , recursosVisiveis
    , nomeDoPapel
    , ehAluno
    , alunos
    , podeTrocarPapel
    , mensagemTrocaDePapel
    , nomeDoRecurso
    ) where

import IHP.Prelude
import Generated.Types

-- Áreas do sistema que podem ser protegidas.
data Recurso
    = Estudantes
    | Cursos
    | Disciplinas
    | Matriculas
    | Notas
    | Relatorios
    | Boletim
    | Usuarios
    deriving (Eq, Show, Enum, Bounded)

-- Visualizar = listar/ver detalhes; Gerenciar = criar, editar e excluir.
data Operacao
    = Visualizar
    | Gerenciar
    deriving (Eq, Show, Enum, Bounded)

--   Recurso      | Admin | Coordenador | Professor | Aluno
--   -------------+-------+-------------+-----------+-------
--   Estudantes   |   G   |      G      |    V*     |   -
--   Cursos       |   G   |      G      |     V     |   V
--   Disciplinas  |   G   |      G      |     V     |   V
--   Matrículas   |   G   |      G      |     -     |   -
--   Notas        |   G   |      V      |     G     |   -
--   Relatórios   |   V   |      V      |     V     |   -
--   Boletim      |   -   |      -      |     -     |   V
--   Usuários     |   G   |      -      |     -     |   -
--
-- (G = gerenciar, que inclui visualizar; V = só visualizar)
-- (*) O professor só enxerga alunos, notas e relatórios das disciplinas que ministra (ver Escopo).
-- Relatórios mostram dados de toda a instituição; o aluno vê apenas o próprio boletim.
podeAcessar :: UserRole -> Operacao -> Recurso -> Bool
podeAcessar Admin _ recurso = recurso /= Boletim
podeAcessar Coordenador operacao recurso = case recurso of
    Usuarios -> False
    Boletim  -> False
    Notas    -> operacao == Visualizar
    _        -> True
podeAcessar Professor operacao recurso = case recurso of
    Usuarios   -> False
    Boletim    -> False
    Matriculas -> False
    Notas    -> True
    _        -> operacao == Visualizar
podeAcessar Aluno operacao recurso =
    operacao == Visualizar && recurso `elem` [Cursos, Disciplinas, Boletim]

-- Recursos que aparecem no menu para um papel (aplicação parcial + filter).
recursosVisiveis :: UserRole -> [Recurso]
recursosVisiveis papel = filter (podeAcessar papel Visualizar) [minBound .. maxBound]

nomeDoPapel :: UserRole -> Text
nomeDoPapel papel = case papel of
    Admin       -> "Administrador"
    Coordenador -> "Coordenador"
    Professor   -> "Professor"
    Aluno       -> "Aluno"

ehAluno :: User -> Bool
ehAluno u = u.userRole == Aluno

alunos :: [User] -> [User]
alunos = filter ehAluno

-- Vínculos: matrículas (aluno) ou disciplinas ministradas (professor).
-- Com vínculos, o papel não pode mudar, senão eles ficariam apontando para o papel errado.
podeTrocarPapel :: UserRole -> UserRole -> Int -> Bool
podeTrocarPapel atual novo vinculos =
    atual == novo || vinculos == 0 || atual `notElem` [Aluno, Professor]

mensagemTrocaDePapel :: UserRole -> Text
mensagemTrocaDePapel papel = case papel of
    Professor -> "Este professor ministra disciplinas. Remova-o das disciplinas antes de trocar o papel."
    _         -> "Este aluno tem matrículas. Exclua as matrículas antes de trocar o papel."

nomeDoRecurso :: UserRole -> Recurso -> Text
nomeDoRecurso papel recurso = case recurso of
    Estudantes  -> if papel == Professor then "Meus alunos" else "Estudantes"
    Cursos      -> "Cursos"
    Disciplinas -> "Disciplinas"
    Matriculas  -> "Matrículas"
    Notas       -> "Notas"
    Relatorios  -> "Relatórios"
    Boletim     -> "Meu boletim"
    Usuarios    -> "Usuários"
