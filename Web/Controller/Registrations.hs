module Web.Controller.Registrations where

import Web.Controller.Prelude
import Web.View.Registrations.New

-- Cadastro público de novos usuários.
-- Toda conta criada por aqui recebe o papel mais restrito (Aluno).
instance Controller RegistrationsController where
    action NewRegistrationAction =
        render NewView { nome = "", email = "" }

    action CreateRegistrationAction = do
        let dados = DadosCadastro
                { nomeCadastro        = param "name"
                , emailCadastro       = param "email"
                , senhaCadastro       = param "password"
                , confirmacaoCadastro = param "passwordConfirmation"
                }

        config <- carregarAuthConfig

        -- Força da senha
        let forcaSenha = case avaliarForcaSenha config dados.senhaCadastro of
                Left erro -> Left (mensagemDaSenha erro)
                Right ()  -> Right ()

        -- E-mails já cadastrados
        usados <- map (.email) <$> fetch (query @User)
        let emailEmUso
                | dados.emailCadastro `elem` usados =
                    Just ("Este e-mail já está em uso. Que tal "
                        <> primeiroEmailLivre usados dados.emailCadastro <> "?")
                | otherwise = Nothing

        pipelineCadastroCPS dados forcaSenha emailEmUso
            (\mensagem -> do
                setErrorMessage mensagem
                render NewView { nome = dados.nomeCadastro, email = dados.emailCadastro })
            (\ok -> do
                hashed <- gerarHashSenha config ok.senhaCadastro
                novoUsuario <- newRecord @User
                    |> set #name ok.nomeCadastro
                    |> set #email ok.emailCadastro
                    |> set #passwordHash hashed
                    |> set #userRole Aluno
                    |> createRecord
                login novoUsuario
                setSuccessMessage "Conta criada com sucesso. Bem-vindo(a)!"
                redirectToPath "/")
