module Application.Helper.View
    ( nomeDoSistema
    , pode
    , formatarNota
    , formatarMedia
    , linkExcluir
    , cabecalhoPagina
    , colunasDeAcao
    , celulasDeAcao
    , badgeStatus
    , badgeSituacao
    ) where

import IHP.ViewPrelude
import Generated.Types
import Web.Types
import Application.Domain
import Text.Printf (printf)

-- Here you can add functions which are available in all your views

nomeDoSistema :: Text
nomeDoSistema = "School Control"

-- O usuário logado pode executar a operação no recurso? (usado para esconder botões/links)
pode operacao recurso = case currentUserOrNothing @User of
    Just usuario -> podeAcessar usuario.userRole operacao recurso
    Nothing      -> False

-- Nota com uma casa decimal e vírgula: 7.25 -> "7,3"
formatarNota :: Double -> Text
formatarNota nota = cs (map virgula (printf "%.1f" nota :: String))
  where
    virgula '.' = ','
    virgula c   = c

-- Média opcional: Nothing -> "—"
formatarMedia :: Maybe Double -> Text
formatarMedia = maybe "—" formatarNota

-- Link de exclusão (o helpers.js do IHP pede confirmação e envia um DELETE).
linkExcluir :: Text -> Html
linkExcluir caminho = [hsx|
    <a href={caminho} class="js-delete text-danger" data-confirm="Tem certeza que deseja excluir?">Excluir</a>
|]

-- Cabeçalhos das colunas Editar/Excluir, exibidos só para quem pode gerenciar.
colunasDeAcao :: Html
colunasDeAcao = [hsx|<th></th><th></th>|]

celulasDeAcao :: Html -> Html -> Html
celulasDeAcao editar excluir = [hsx|<td>{editar}</td><td>{excluir}</td>|]

-- Cabeçalho de página: título à esquerda e uma ação opcional (ex.: botão "+ Novo") à direita.
cabecalhoPagina :: Text -> Html -> Html
cabecalhoPagina titulo acao = [hsx|
    <div class="sga-page-head">
        <h1 class="h3 mb-0">{titulo}</h1>
        <div class="sga-page-actions">{acao}</div>
    </div>
|]

-- Badge colorido para o status da matrícula.
badgeStatus :: EnrollmentStatus -> Html
badgeStatus status = [hsx|<span class={"badge " <> cor}>{nomeDoStatus status}</span>|]
    where
        cor :: Text
        cor = case status of
            Ativa     -> "bg-success"
            Trancada  -> "bg-warning text-dark"
            Concluida -> "bg-primary"

-- Badge colorido para a situação do aluno (boletim).
badgeSituacao :: Situacao -> Html
badgeSituacao situacao = [hsx|<span class={"badge " <> cor}>{nomeDaSituacao situacao}</span>|]
    where
        cor :: Text
        cor = case situacao of
            Aprovado      -> "bg-success"
            Reprovado     -> "bg-danger"
            AguardandoAV3 -> "bg-warning text-dark"
            Trancado      -> "bg-secondary"


-- Opções dos <select> dos formulários

instance CanSelect User where
    type SelectValue User = Id User
    selectValue user = user.id
    selectLabel user = user.name

instance CanSelect (Subject, Course) where
    type SelectValue (Subject, Course) = Id Subject
    selectValue (subject, _) = subject.id
    selectLabel = rotuloDaDisciplina

instance CanSelect Course where
    type SelectValue Course = Id Course
    selectValue course = course.id
    selectLabel course = course.name

instance CanSelect EnrollmentStatus where
    type SelectValue EnrollmentStatus = EnrollmentStatus
    selectValue status = status
    selectLabel = nomeDoStatus

instance CanSelect UserRole where
    type SelectValue UserRole = UserRole
    selectValue papel = papel
    selectLabel = nomeDoPapel

instance CanSelect OpcaoMatricula where
    type SelectValue OpcaoMatricula = Id Enrollment
    selectValue opcao = opcao.matriculaId
    selectLabel opcao = opcao.rotulo
