module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import Generated.Types
import Web.Types
import Web.Routes
import Application.Helper.View
import Application.Domain

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html lang="pt-BR">
    <head>
        {metaTags}

        {stylesheets}
        {scripts}

        <title>{pageTitleOrDefault "SGA-Func"}</title>
    </head>
    <body>
        {navbar}
        <div class="container mt-4">
            {renderFlashMessages}
            {inner}
        </div>
    </body>
</html>
|]

-- O menu mostra só as áreas que o papel do usuário logado pode visualizar.
navbar :: Html
navbar = [hsx|
    <nav class="navbar navbar-expand navbar-dark bg-dark">
        <div class="container">
            <a class="navbar-brand" href="/">SGA-Func</a>
            <div class="navbar-nav me-auto">
                {forEach recursos linkDoMenu}
            </div>
            {usuarioLogado}
        </div>
    </nav>
|]
    where
        recursos = maybe [] (\usuario -> recursosVisiveis usuario.userRole) (currentUserOrNothing @User)

        usuarioLogado = case currentUserOrNothing @User of
            Just usuario -> [hsx|
                <div class="navbar-nav align-items-center">
                    <span class="navbar-text me-3">
                        {usuario.name} <span class="badge bg-secondary">{nomeDoPapel usuario.userRole}</span>
                    </span>
                    <a class="nav-link js-delete js-delete-no-confirm" href={DeleteSessionAction}>Sair</a>
                </div>
            |]
            Nothing -> [hsx|
                <div class="navbar-nav">
                    <a class="nav-link" href={NewSessionAction}>Entrar</a>
                </div>
            |]

linkDoMenu :: Recurso -> Html
linkDoMenu recurso = case recurso of
    Estudantes -> [hsx|<a class="nav-link" href={StudentsAction}>Estudantes</a>|]
    Cursos     -> [hsx|<a class="nav-link" href={CoursesAction}>Cursos</a>|]
    Matriculas -> [hsx|<a class="nav-link" href={EnrollmentsAction}>Matrículas</a>|]
    Notas      -> [hsx|<a class="nav-link" href={GradesAction}>Notas</a>|]
    Relatorios -> [hsx|<a class="nav-link" href={ReportsAction}>Relatórios</a>|]
    Usuarios   -> [hsx|<a class="nav-link" href={UsersAction}>Usuários</a>|]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-4.0.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
    {autoRefreshMeta}
|]
