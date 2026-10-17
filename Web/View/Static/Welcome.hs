module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = case currentUserOrNothing @User of
        Nothing      -> visitante
        Just usuario -> painel usuario

-- Página para quem ainda não entrou
visitante :: Html
visitante = [hsx|
    <div class="p-5 mb-4 bg-light rounded-3">
        <h1 class="display-6">SGA-Func</h1>
        <p class="lead">Sistema de gestão acadêmica: alunos, cursos, matrículas e notas.</p>
        <a href={NewSessionAction} class="btn btn-primary btn-lg">Entrar</a>
    </div>
|]

-- Atalhos para as áreas que o papel do usuário pode ver
painel :: User -> Html
painel usuario = [hsx|
    <h1 class="h3">Olá, {usuario.name}!</h1>
    <p class="text-muted">Você entrou como <strong>{nomeDoPapel usuario.userRole}</strong>.</p>
    <div class="row g-3">
        {forEach (recursosVisiveis usuario.userRole) atalho}
    </div>
|]

atalho :: Recurso -> Html
atalho recurso = [hsx|
    <div class="col-sm-6 col-lg-4">
        <a href={destino} class="card text-decoration-none h-100">
            <div class="card-body">
                <h2 class="h5 card-title">{titulo}</h2>
                <p class="card-text text-muted">{descricao}</p>
            </div>
        </a>
    </div>
|]
    where
        titulo, descricao, destino :: Text
        (titulo, descricao, destino) = case recurso of
            Estudantes -> ("Estudantes", "Cadastro de alunos", pathTo StudentsAction)
            Cursos     -> ("Cursos", "Cursos e carga horária", pathTo CoursesAction)
            Matriculas -> ("Matrículas", "Alunos matriculados em cada curso", pathTo EnrollmentsAction)
            Notas      -> ("Notas", "Lançamento de PE/AVf e situação final", pathTo GradesAction)
            Relatorios -> ("Relatórios", "Médias, ranking e alunos por curso", pathTo ReportsAction)
            Usuarios   -> ("Usuários", "Contas de acesso e papéis", pathTo UsersAction)
