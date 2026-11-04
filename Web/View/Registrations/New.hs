module Web.View.Registrations.New where
import Web.View.Prelude

-- Guarda o que o visitante já digitou (menos a senha) para repreencher em caso de erro.
data NewView = NewView
    { nome  :: Text
    , email :: Text
    }

instance View NewView where
    html NewView { .. } = [hsx|
        <div class="row justify-content-center mt-5" id="registrations-new">
            <div class="col-md-6 col-lg-5">
                <div class="card shadow-sm">
                    <div class="card-body p-4">
                        <div class="sga-auth-head">
                            <span class="sga-auth-mark">SGA</span>
                            <h1 class="h4 mb-1">Criar conta no SGA-Func</h1>
                            <p class="sga-auth-sub mb-0">Crie sua conta de aluno em segundos</p>
                        </div>
                        {renderForm nome email}
                        <p class="text-center text-muted mt-3 mb-0">
                            Já tem conta? <a href={NewSessionAction}>Entrar</a>
                        </p>
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: Text -> Text -> Html
renderForm nome email = [hsx|
    <form method="POST" action={CreateRegistrationAction}>
        <div class="mb-3">
            <label class="form-label" for="name">Nome</label>
            <input id="name" name="name" value={nome} type="text" class="form-control" required="required" autofocus="autofocus" />
        </div>
        <div class="mb-3">
            <label class="form-label" for="email">E-mail</label>
            <input id="email" name="email" value={email} type="email" class="form-control" required="required" />
        </div>
        <div class="mb-3">
            <label class="form-label" for="password">Senha</label>
            <input id="password" name="password" type="password" class="form-control" required="required" />
            <div class="form-text">Mínimo de 8 caracteres, com ao menos uma letra e um número.</div>
        </div>
        <div class="mb-3">
            <label class="form-label" for="passwordConfirmation">Confirmar senha</label>
            <input id="passwordConfirmation" name="passwordConfirmation" type="password" class="form-control" required="required" />
        </div>
        <button type="submit" class="btn btn-primary w-100">Criar conta</button>
    </form>
|]
