module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) where
    html NewView { .. } = [hsx|
        <div class="row justify-content-center mt-5" id="sessions-new">
            <div class="col-md-5 col-lg-4">
                <div class="card shadow-sm">
                    <div class="card-body p-4">
                        <h1 class="h4 mb-4 text-center">Entrar no SGA-Func</h1>
                        {renderForm user}
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
