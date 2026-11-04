module Application.Helper.View
    ( pode
    , formatarNota
    , formatarMedia
    , linkExcluir
    , cabecalhoPagina
    , badgeStatus
    , badgeSituacao
    ) where

import IHP.ViewPrelude
import Generated.Types
import Web.Types
import Application.Domain
import Text.Printf (printf)

-- Here you can add functions which are available in all your views

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

instance CanSelect Student where
    type SelectValue Student = Id Student
    selectValue student = student.id
    selectLabel student = student.name

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
