module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = case currentUserOrNothing @User of
        Nothing      -> visitante
        Just usuario -> painel usuario

-- Página para quem ainda não entrou: hero com a proposta + vitrine de recursos.
visitante :: Html
visitante = [hsx|
    <section class="sga-hero">
        <span class="sga-hero-eyebrow">Gestão acadêmica</span>
        <h1 class="sga-hero-title">Alunos, cursos, matrículas e notas num só lugar</h1>
        <p class="sga-hero-subtitle">
            O {nomeDoSistema} centraliza o controle acadêmico — com cálculo de médias,
            situação de cada aluno e relatórios — de um jeito simples e organizado.
        </p>
        <div class="sga-hero-actions">
            <a href={NewSessionAction} class="btn btn-primary btn-lg">Entrar</a>
            <a href={NewRegistrationAction} class="btn btn-outline-primary btn-lg">Criar conta</a>
        </div>
    </section>

    <div class="row g-4 sga-features">
        {forEach vitrine cartaoVitrine}
    </div>
|]
    where
        vitrine :: [(Text, Text, Text)]
        vitrine =
            [ ("🎓", "Estudantes", "Cadastro e acompanhamento dos alunos")
            , ("📚", "Cursos", "Cursos com carga horária")
            , ("📖", "Disciplinas", "Disciplinas que compõem cada curso")
            , ("🧾", "Matrículas", "Vínculo de alunos às disciplinas")
            , ("📝", "Notas", "Lançamento de PE/AVf e situação final")
            , ("📊", "Relatórios", "Médias, ranking e alunos por curso")
            ]

cartaoVitrine :: (Text, Text, Text) -> Html
cartaoVitrine (icone, titulo, descricao) = [hsx|
    <div class="col-sm-6 col-lg-4">
        <div class="card h-100">
            <div class="card-body">
                <span class="sga-feature-icon">{icone}</span>
                <h2 class="h5 card-title">{titulo}</h2>
                <p class="card-text text-muted mb-0">{descricao}</p>
            </div>
        </div>
    </div>
|]

-- Painel de quem está logado: saudação + atalhos para as áreas visíveis ao papel.
painel :: User -> Html
painel usuario = [hsx|
    <div class="sga-greeting">
        <h1 class="h3">Olá, {usuario.name}! 👋</h1>
        <p>Você entrou como <span class="badge bg-secondary">{nomeDoPapel usuario.userRole}</span></p>
    </div>
    <div class="row g-3">
        {forEach (recursosVisiveis usuario.userRole) (atalho usuario.userRole)}
    </div>
|]

atalho :: UserRole -> Recurso -> Html
atalho papel recurso = [hsx|
    <div class="col-sm-6 col-lg-4">
        <a href={destino} class="card text-decoration-none h-100">
            <div class="card-body">
                <span class="sga-card-icon">{icone}</span>
                <h2 class="h5 card-title">{titulo}</h2>
                <p class="card-text text-muted mb-0">{descricao}</p>
            </div>
        </a>
    </div>
|]
    where
        icone, titulo, descricao :: Text
        destino :: Text
        (icone, titulo, descricao, destino) = case recurso of
            Estudantes
                | papel == Professor -> ("🎓", "Meus alunos", "Alunos matriculados nas suas disciplinas", pathTo StudentsAction)
                | otherwise          -> ("🎓", "Estudantes", "Cadastro de alunos", pathTo StudentsAction)
            Cursos     -> ("📚", "Cursos", "Cursos e carga horária", pathTo CoursesAction)
            Disciplinas -> ("📖", "Disciplinas", "Disciplinas de cada curso", pathTo SubjectsAction)
            Matriculas -> ("🧾", "Matrículas", "Alunos matriculados em cada disciplina", pathTo EnrollmentsAction)
            Notas      -> ("📝", "Notas", "Lançamento de PE/AVf e situação final", pathTo GradesAction)
            Relatorios -> ("📊", "Relatórios", "Médias, ranking e alunos por curso", pathTo ReportsAction)
            Boletim    -> ("🗒️", "Meu boletim", "Suas disciplinas, notas e situação", pathTo MeuBoletimAction)
            Usuarios   -> ("👥", "Usuários", "Contas de acesso e papéis", pathTo UsersAction)
