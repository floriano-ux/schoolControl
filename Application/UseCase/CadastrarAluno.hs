{-# LANGUAGE NoFieldSelectors #-}
module Application.UseCase.CadastrarAluno
    ( NovoAluno (..)
    , ErroCadastroAluno (..)
    , PortasCadastroAluno (..)
    , cadastrarAluno
    , mensagemErroCadastroAluno
    ) where

import IHP.Prelude
import Generated.Types
import Application.Domain.Validacao (emailValido)
import qualified Data.Text as Text

data NovoAluno = NovoAluno
    { nome  :: Text
    , email :: Text
    } deriving (Eq, Show)

data ErroCadastroAluno
    = NomeVazio
    | EmailInvalidoAluno
    | EmailJaCadastrado
    deriving (Eq, Show)

-- Portas: as dependências de efeito entram como um record de funções.
data PortasCadastroAluno m = PortasCadastroAluno
    { emailJaExiste :: Text -> m Bool
    , gerarSenha    :: m Text
    , gerarHash     :: Text -> m Text
    , persistir     :: NovoAluno -> Text -> m User
    }

-- Devolve o usuário criado e a senha em texto puro, para ser exibida uma única vez.
cadastrarAluno
    :: Monad m
    => PortasCadastroAluno m
    -> NovoAluno
    -> m (Either ErroCadastroAluno (User, Text))
cadastrarAluno portas dados =
    case validar dados of
        Left erro -> pure (Left erro)
        Right d -> do
            existe <- portas.emailJaExiste d.email
            if existe
                then pure (Left EmailJaCadastrado)
                else do
                    senha <- portas.gerarSenha
                    hash  <- portas.gerarHash senha
                    user  <- portas.persistir d hash
                    pure (Right (user, senha))

validar :: NovoAluno -> Either ErroCadastroAluno NovoAluno
validar dados
    | Text.null nome          = Left NomeVazio
    | not (emailValido email) = Left EmailInvalidoAluno
    | otherwise               = Right NovoAluno { nome, email }
  where
    nome  = Text.strip dados.nome
    email = Text.strip dados.email

mensagemErroCadastroAluno :: ErroCadastroAluno -> Text
mensagemErroCadastroAluno erro = case erro of
    NomeVazio          -> "Informe o nome do estudante"
    EmailInvalidoAluno -> "Informe um e-mail válido"
    EmailJaCadastrado  -> "Este e-mail já está cadastrado"
