module Web.View.Students.Show where
import Web.View.Prelude

data ShowView = ShowView { student :: User }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{student.name}</h1>
        <dl>
            <dt>Nome</dt><dd>{student.name}</dd>
            <dt>E-mail</dt><dd>{student.email}</dd>
        </dl>
        {when (pode Gerenciar Estudantes) regenerarSenha}
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Estudantes" StudentsAction
                            , breadcrumbText "Detalhes do estudante"
                            ]
            regenerarSenha = [hsx|
                <form method="POST" action={RegenerarSenhaAction student.id}
                      onsubmit="return confirm('Gerar uma nova senha? A senha atual deixará de funcionar.')">
                    <button type="submit" class="btn btn-outline-primary">Gerar nova senha</button>
                </form>
            |]
