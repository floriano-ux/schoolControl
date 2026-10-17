module Application.Helper.View
    ( pode
    , formatarNota
    , formatarMedia
    , linkExcluir
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
