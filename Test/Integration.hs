module Main where

-- Testes de integração (Sprint 5): exercitam o domínio, a segurança e o
-- tratamento de erros monádico CONTRA UM POSTGRESQL DE VERDADE.
--
-- Como roda:
--   * CI / `nix flake check`: o IHP sobe um PostgreSQL temporário e define DATABASE_URL.
--   * Local (dentro do `devenv up`/direnv): DATABASE_URL aponta para o banco de dev.
--
-- Em ambos os casos o teste NÃO toca no banco apontado: ele cria (e recria do zero)
-- um banco separado, `schoolcontrol_integracao`, carrega Application/Schema.sql e
-- Application/Fixtures.sql nele via `psql`, e só então executa os cenários.

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder (query, filterWhere)
import IHP.Fetch (fetch, fetchCount, fetchOne)
import Generated.Types
import Application.Domain
import Application.Errors
import Application.Security
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import System.Process (callProcess)
import System.IO (hSetEncoding, stdout, stderr, utf8)

nomeDoBancoDeTeste :: Text
nomeDoBancoDeTeste = "schoolcontrol_integracao"

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    urlOriginal <- lookupEnv "DATABASE_URL" >>= \case
        Just url -> pure (cs url :: Text)
        Nothing -> error "DATABASE_URL não definida. Rode dentro do `devenv up`/direnv ou use `nix flake check`."

    let urlTeste = trocarBanco nomeDoBancoDeTeste urlOriginal
    prepararBanco urlOriginal urlTeste

    withModelContext (cs urlTeste) noopLogger \modelContext -> do
        let ?modelContext = modelContext
        hspec specs

-- | Troca o nome do banco numa URL do tipo "postgresql:///app?host=/tmp/x".
trocarBanco :: Text -> Text -> Text
trocarBanco nome url = prefixo <> nome <> consulta
  where
    (base, consulta) = Text.breakOn "?" url
    prefixo = Text.dropWhileEnd (/= '/') base

-- | Recria o banco de teste e carrega schema + fixtures com o psql.
prepararBanco :: Text -> Text -> IO ()
prepararBanco urlOriginal urlTeste = do
    psql urlOriginal ["-c", "DROP DATABASE IF EXISTS " <> nomeDoBancoDeTeste <> " WITH (FORCE)"]
    psql urlOriginal ["-c", "CREATE DATABASE " <> nomeDoBancoDeTeste]
    -- uuid_generate_v4 vem da extensão uuid-ossp (como no IHPSchema.sql); se ela não
    -- existir no PostgreSQL disponível, cai para gen_random_uuid (nativo no PG 13+).
    psql urlTeste ["-c", Text.unwords
        [ "DO $$ BEGIN CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";"
        , "EXCEPTION WHEN OTHERS THEN"
        , "CREATE OR REPLACE FUNCTION uuid_generate_v4() RETURNS uuid AS 'SELECT gen_random_uuid()' LANGUAGE sql;"
        , "END $$;" ]]
    psql urlTeste ["-f", "Application/Schema.sql"]
    psql urlTeste ["-f", "Application/Fixtures.sql"]
  where
    psql :: Text -> [Text] -> IO ()
    psql url argumentos = callProcess "psql"
        (["-v", "ON_ERROR_STOP=1", "-q", cs url] <> map cs argumentos)

-- ---------------------------------------------------------------------------
-- Fábricas de registros (cada teste usa e-mails próprios para não colidir)
-- ---------------------------------------------------------------------------

criarAluno :: (?modelContext :: ModelContext) => Text -> IO Student
criarAluno nome = newRecord @Student
    |> set #name nome
    |> set #email (Text.toLower (Text.replace " " "." nome) <> "@teste.dev")
    |> createRecord

criarCurso :: (?modelContext :: ModelContext) => Text -> Int -> IO Course
criarCurso nome carga = newRecord @Course
    |> set #name nome
    |> set #workload carga
    |> createRecord

matricular :: (?modelContext :: ModelContext) => Student -> Course -> IO Enrollment
matricular aluno curso = novaMatricula aluno curso |> createRecord

novaMatricula :: Student -> Course -> Enrollment
novaMatricula aluno curso = newRecord @Enrollment
    |> set #studentId aluno.id
    |> set #courseId curso.id
    |> set #status Ativa

lancarNotas :: (?modelContext :: ModelContext) => Enrollment -> Double -> IO Grade
lancarNotas matricula valor = newRecord @Grade
    |> set #enrollmentId matricula.id
    |> set #pe1 valor |> set #avf1 valor
    |> set #pe2 valor |> set #avf2 valor
    |> set #pe3 (Just valor) |> set #avf3 (Just valor)
    |> createRecord

ehConflito, ehValidacao :: Either AppError a -> Bool
ehConflito = \case Left (Conflito _) -> True; _ -> False
ehValidacao = \case Left (ErroDeValidacao _) -> True; _ -> False

-- ---------------------------------------------------------------------------
-- Cenários
-- ---------------------------------------------------------------------------

specs :: (?modelContext :: ModelContext) => Spec
specs = do
    describe "Banco e schema" do
        it "aceita consultas e tem as fixtures de usuários carregadas" do
            total <- query @User |> fetchCount
            total `shouldBe` 4

    describe "Matrícula: domínio + banco + AppM" do
        it "grava aluno, curso e matrícula e lê de volta" do
            aluno <- criarAluno "Ana Integracao"
            curso <- criarCurso "Haskell Avancado" 60
            matricula <- matricular aluno curso
            lida <- fetchOne matricula.id
            lida.studentId `shouldBe` aluno.id
            lida.courseId `shouldBe` curso.id
            lida.status `shouldBe` Ativa

        it "pipelineMatriculaCPS detecta duplicidade usando os dados reais do banco" do
            aluno <- criarAluno "Bruno Integracao"
            curso <- criarCurso "SQL Integracao" 40
            _ <- matricular aluno curso
            alunos <- query @Student |> fetch
            cursos <- query @Course |> fetch
            matriculas <- query @Enrollment |> fetch
            pipelineMatriculaCPS (novaMatricula aluno curso) alunos cursos matriculas Left Right
                `shouldBe` Left MatriculaDuplicada

        it "corrida: se a validação passar mas o INSERT colidir, o UNIQUE vira Conflito (não erro 500)" do
            aluno <- criarAluno "Carla Integracao"
            curso <- criarCurso "Redes Integracao" 30
            _ <- matricular aluno curso
            resultado <- executarApp (capturarInfra "teste/matricula" (matricular aluno curso))
            resultado `shouldSatisfy` ehConflito

        it "chave estrangeira inválida vira ErroDeValidacao" do
            aluno <- criarAluno "Davi Integracao"
            let fantasma = newRecord @Enrollment
                    |> set #studentId aluno.id
                    |> set #courseId "00000000-0000-0000-0000-00000000dead"
            resultado <- executarApp (capturarInfra "teste/fk" (createRecord fantasma))
            resultado `shouldSatisfy` ehValidacao

        it "excluir o aluno remove as matrículas em cascata" do
            aluno <- criarAluno "Eva Integracao"
            curso <- criarCurso "Cascata Integracao" 20
            _ <- matricular aluno curso
            deleteRecord aluno
            restantes <- query @Enrollment |> filterWhere (#studentId, aluno.id) |> fetchCount
            restantes `shouldBe` 0

    describe "Restrições CHECK do banco reforçam as regras do domínio" do
        it "carga horária zero é rejeitada pelo banco" do
            resultado <- executarApp (capturarInfra "teste/check" (criarCurso "Curso Invalido" 0))
            resultado `shouldSatisfy` ehValidacao

        it "nota acima de 10 é rejeitada pelo banco" do
            aluno <- criarAluno "Fabio Integracao"
            curso <- criarCurso "Notas Integracao" 60
            matricula <- matricular aluno curso
            resultado <- executarApp (capturarInfra "teste/nota" (lancarNotas matricula 11))
            resultado `shouldSatisfy` ehValidacao

    describe "Relatórios sobre dados persistidos" do
        it "mediaPorCurso e rankingDeAlunos calculam a partir do que foi gravado" do
            curso <- criarCurso "Relatorio Integracao" 60
            gil <- criarAluno "Gil Integracao"
            hel <- criarAluno "Hel Integracao"
            mg <- matricular gil curso
            mh <- matricular hel curso
            _ <- lancarNotas mg 9
            _ <- lancarNotas mh 7
            alunos <- query @Student |> fetch
            matriculas <- query @Enrollment |> filterWhere (#courseId, curso.id) |> fetch
            notas <- query @Grade |> fetch
            mediaPorCurso [curso] matriculas notas `shouldBe` [("Relatorio Integracao", Just 8)]
            map (\(posicao, nome, _) -> (posicao, nome)) (rankingDeAlunos alunos matriculas notas)
                `shouldBe` [(1, "Gil Integracao"), (2, "Hel Integracao")]

    describe "Segurança com persistência" do
        it "as senhas das fixtures estão em bcrypt e conferem com 'senha123'" do
            admin <- query @User |> filterWhere (#email, "admin@schoolcontrol.dev") |> fetchOne
            admin.passwordHash `shouldSatisfy` Text.isPrefixOf "$2"
            conferirSenha "senha123" admin.passwordHash `shouldBe` True
            conferirSenha "errada123" admin.passwordHash `shouldBe` False

        it "CPF é gravado cifrado e só é legível com a chave" do
            let config = configPadrao { bcryptCost = 4 }
            hash <- gerarHashSenha config "senha123"
            cpfCifrado <- cifrarOpcional config "123.456.789-00"
            usuario <- newRecord @User
                |> set #name "Ivo Integracao"
                |> set #email "ivo@teste.dev"
                |> set #passwordHash hash
                |> set #document cpfCifrado
                |> createRecord
            lido <- fetchOne usuario.id
            lido.document `shouldNotBe` Just "123.456.789-00"
            decifrarOpcional config lido.document `shouldBe` Just "123.456.789-00"

        it "e-mail de usuário duplicado vira Conflito" do
            hash <- gerarHashSenha configPadrao { bcryptCost = 4 } "senha123"
            let duplicado = newRecord @User
                    |> set #name "Outro Admin"
                    |> set #email "admin@schoolcontrol.dev"
                    |> set #passwordHash hash
            resultado <- executarApp (capturarInfra "teste/usuario" (createRecord duplicado))
            resultado `shouldSatisfy` ehConflito
