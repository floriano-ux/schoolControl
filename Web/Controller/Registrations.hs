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

        -- Reexibe o formulário com uma mensagem amigável (e repreenche o que foi digitado).
        let reexibirFormulario mensagem = do
                setErrorMessage mensagem
                render NewView { nome = dados.nomeCadastro, email = dados.emailCadastro }

        -- Força da senha (Reader, em Application.Security), traduzida para mensagem.
        let forcaSenha = case avaliarForcaSenha config dados.senhaCadastro of
                Left erro -> Left (mensagemDaSenha erro)
                Right ()  -> Right ()

        -- Leitura dos e-mails já usados (ponto que pode falhar por problema de banco).
        usadosResult <- tentar "cadastro" (map (.email) <$> fetch (query @User))
        case usadosResult of
            Left mensagem -> reexibirFormulario mensagem
            Right usados -> do
                let emailEmUso
                        | dados.emailCadastro `elem` usados =
                            Just ("Este e-mail já está em uso. Que tal "
                                <> primeiroEmailLivre usados dados.emailCadastro <> "?")
                        | otherwise = Nothing

                pipelineCadastroCPS dados forcaSenha emailEmUso
                    (\mensagem -> do
                        logAuthWarn ("cadastro recusado para " <> dados.emailCadastro <> ": " <> mensagem)
                        reexibirFormulario mensagem)
                    (\ok -> do
                        criado <- tentar "cadastro" do
                            hashed <- gerarHashSenha config ok.senhaCadastro
                            newRecord @User
                                |> set #name ok.nomeCadastro
                                |> set #email ok.emailCadastro
                                |> set #passwordHash hashed
                                |> set #userRole Aluno
                                |> createRecord
                        case criado of
                            Left mensagem -> reexibirFormulario mensagem
                            Right novoUsuario -> do
                                login novoUsuario
                                logAuthInfo ("nova conta criada: " <> ok.emailCadastro)
                                setSuccessMessage "Conta criada com sucesso. Bem-vindo(a)!"
                                redirectToPath "/")
