module Web.Controller.Students where

import Web.Controller.Prelude
import Web.View.Students.Index
import Web.View.Students.New
import Web.View.Students.Edit
import Web.View.Students.Show
import Web.View.Students.Credenciais
import Application.UseCase.CadastrarAluno
import Application.Security.GeradorSenha (gerarSenhaAleatoria)
import "crypton" Crypto.Random (getRandomBytes)

-- Estudantes são uma fachada sobre users com papel aluno.
instance Controller StudentsController where
    beforeAction = exigirPermissao Visualizar Estudantes

    action StudentsAction = do
        todos      <- query @User |> filterWhere (#userRole, Aluno) |> orderBy #name |> fetch
        matriculas <- matriculasVisiveisPara currentUser
        let students = alunosVisiveis currentUser matriculas todos
        render IndexView { .. }

    action NewStudentAction = do
        exigirPermissao Gerenciar Estudantes
        let student = newRecord @User
        render NewView { .. }

    action ShowStudentAction { studentId } = do
        student <- buscarAluno studentId
        render ShowView { .. }

    action EditStudentAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- buscarAluno studentId
        render EditView { .. }

    action UpdateStudentAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- buscarAluno studentId
        student
            |> fill @'["name", "email"]
            |> set #userRole Aluno
            |> validateField #name (nonEmpty |> withCustomErrorMessage "Informe o nome do estudante")
            |> validateField #email (isEmail |> withCustomErrorMessage "Informe um e-mail válido")
            |> validarEmailUnico
            >>= ifValid \case
                Left student -> render EditView { .. }
                Right student -> do
                    student <- student |> updateRecord
                    setSuccessMessage "Estudante atualizado"
                    redirectTo EditStudentAction { .. }

    action CreateStudentAction = do
        exigirPermissao Gerenciar Estudantes
        config <- carregarAuthConfig
        let dados = NovoAluno { nome = param "name", email = param "email" }
            portas = PortasCadastroAluno
                { emailJaExiste = \email -> query @User |> filterWhereCaseInsensitive (#email, email) |> fetchExists
                , gerarSenha    = gerarSenhaAleatoria getRandomBytes config
                , gerarHash     = gerarHashSenha config
                , persistir     = \novo hash -> newRecord @User
                    |> set #name novo.nome
                    |> set #email novo.email
                    |> set #passwordHash hash
                    |> set #userRole Aluno
                    |> createRecord
                }
            formulario = newRecord @User |> set #name dados.nome |> set #email dados.email
        resultado <- tentar "estudantes" (cadastrarAluno portas dados)
        case resultado of
            Left mensagem -> do
                setErrorMessage mensagem
                render NewView { student = formulario }
            Right (Left erro) ->
                render NewView { student = formulario |> anexarErro erro }
            Right (Right (aluno, senha)) -> do
                logAuthInfo ("aluno cadastrado pelo painel: " <> aluno.email)
                render CredenciaisView { student = aluno, senha }

    action RegenerarSenhaAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- buscarAluno studentId
        config <- carregarAuthConfig
        senha <- gerarSenhaAleatoria getRandomBytes config
        hash <- gerarHashSenha config senha
        student <- student
            |> set #passwordHash hash
            |> set #failedLoginAttempts 0
            |> set #lockedAt Nothing
            |> updateRecord
        logAuthInfo ("senha regenerada para o aluno: " <> student.email)
        render CredenciaisView { student, senha }

    action DeleteStudentAction { studentId } = do
        exigirPermissao Gerenciar Estudantes
        student <- buscarAluno studentId
        deleteRecord student
        setSuccessMessage "Estudante excluído"
        redirectTo StudentsAction

-- Só alunos aparecem aqui; o professor só acessa alunos matriculados nas suas disciplinas.
buscarAluno :: (?modelContext :: ModelContext, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => Id User -> IO User
buscarAluno userId = do
    user <- fetch userId
    notFoundWhen (not (ehAluno user))
    matriculas <- matriculasVisiveisPara currentUser
    notFoundWhen (null (alunosVisiveis currentUser matriculas [user]))
    pure user

anexarErro :: ErroCadastroAluno -> User -> User
anexarErro erro = case erro of
    NomeVazio          -> attachFailure #name mensagem
    EmailInvalidoAluno -> attachFailure #email mensagem
    EmailJaCadastrado  -> attachFailure #email mensagem
  where
    mensagem = mensagemErroCadastroAluno erro

validarEmailUnico student = student
    |> withCustomErrorMessageIO "Este e-mail já está cadastrado" validateIsUnique #email
