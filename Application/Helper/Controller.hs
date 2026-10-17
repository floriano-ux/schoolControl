module Application.Helper.Controller
    ( viaDominio
    , exigirPermissao
    , opcoesDeMatricula
    ) where

import IHP.ControllerPrelude
import Generated.Types
import Web.Types
import Web.Routes ()
import Application.Domain

-- Here you can add functions which are available in all your controllers

-- Adapta uma validação pura do domínio (Either) para um validador do IHP.
-- Ex.: validateField #workload (viaDominio validarCargaHoraria)
viaDominio :: (valor -> Either ErroValidacao valor) -> valor -> ValidatorResult
viaDominio validar valor = case validar valor of
    Right _   -> Success
    Left erro -> Failure (mensagemDeErro erro)

-- Garante que há um usuário logado e que o papel dele permite a operação.
-- Sem login: redireciona para a tela de login (e volta para cá depois de entrar).
-- Sem permissão: volta para a página inicial com uma mensagem de erro.
-- No IHP 1.6 as actions devolvem IO ResponseReceived; para "sair no meio" de uma
-- action usamos earlyReturn, como faz o próprio ensureIsUser.
exigirPermissao operacao recurso = case currentUserOrNothing @User of
    Nothing -> do
        setErrorMessage "Faça login para acessar esta página."
        setSession "IHP.LoginSupport.redirectAfterLogin" getRequestPathAndQuery
        earlyReturn (redirectTo NewSessionAction)
    Just usuario ->
        unless (podeAcessar usuario.userRole operacao recurso) do
            setErrorMessage "Você não tem permissão para acessar esta página."
            earlyReturn (redirectToPath "/")

-- Opções do select de matrícula, em ordem alfabética: "Aluno — Curso (status)".
opcoesDeMatricula :: [Student] -> [Course] -> [Enrollment] -> [OpcaoMatricula]
opcoesDeMatricula students courses enrollments =
    sortOn (.rotulo) (map opcao enrollments)
  where
    opcao enrollment = OpcaoMatricula
        { matriculaId = enrollment.id
        , rotulo = nomeDoAluno students enrollment.studentId
            <> " — " <> nomeDoCurso courses enrollment.courseId
            <> " (" <> nomeDoStatus enrollment.status <> ")"
        }
