module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.LoginSupport.Middleware
import Generated.Types
import Web.Types () -- traz a instância CurrentUserRecord = User

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here

    -- Carrega o usuário logado (currentUser) em cada requisição
    option $ AuthMiddleware (authMiddleware @User)
