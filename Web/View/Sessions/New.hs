module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) where
    html NewView { .. } = [hsx|
        <div class="row justify-content-center mt-5" id="sessions-new">
            <div class="col-md-5 col-lg-4">
                <div class="card shadow-sm">
                    <div class="card-body p-4">
                        <div class="sga-auth-head">
                            <span class="sga-auth-mark">SGA</span>
                            <h1 class="h4 mb-1">Entrar no SGA-Func</h1>
                            <p class="sga-auth-sub mb-0">Acesse o painel de gestão acadêmica</p>
                        </div>
                        {renderForm user}
                        <p class="text-center text-muted mt-3 mb-0">
                            Não tem conta? <a href={NewRegistrationAction}>Criar conta</a>
                        </p>
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Html
renderForm user = [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="mb-3">
            <label class="form-label" for="email">E-mail</label>
            <input id="email" name="email" value={user.email} type="email" class="form-control" required="required" autofocus="autofocus" />
        </div>
        <div class="mb-3">
            <label class="form-label" for="password">Senha</label>
            <input id="password" name="password" type="password" class="form-control" required="required"/>
        </div>
        <button type="submit" class="btn btn-primary w-100">Entrar</button>
    </form>
|]
