module Main where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Domain
import Application.Errors
import Application.Security
import qualified Data.Text as Text
import System.IO (hSetEncoding, stdout, stderr, utf8)

-- Monta nota sem tocar no banco TESTEEES
nota :: Double -> Double -> Maybe Double -> Grade
nota av1 av2 av3 = newRecord @Grade
    |> set #pe1 av1 |> set #avf1 av1
    |> set #pe2 av2 |> set #avf2 av2
    |> set #pe3 av3 |> set #avf3 av3

main :: IO ()
main = do
    -- Os nomes dos testes têm acentos; garante UTF-8 mesmo em locale "C" (CI)
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hspec specs

-- Dados de exemplo para os relatórios (Sprint 2), montados sem banco.
cursoHaskell, cursoSQL :: Course
cursoHaskell = newRecord @Course |> set #id "00000000-0000-0000-0000-0000000000c1" |> set #name "Haskell" |> set #workload 60
cursoSQL     = newRecord @Course |> set #id "00000000-0000-0000-0000-0000000000c2" |> set #name "SQL" |> set #workload 40

ana, bruno :: Student
ana   = newRecord @Student |> set #id "00000000-0000-0000-0000-0000000000a1" |> set #name "Ana"
bruno = newRecord @Student |> set #id "00000000-0000-0000-0000-0000000000b1" |> set #name "Bruno"

matricula :: Id Enrollment -> Student -> Course -> EnrollmentStatus -> Enrollment
matricula matriculaId student course status = newRecord @Enrollment
    |> set #id matriculaId |> set #studentId student.id |> set #courseId course.id |> set #status status

m1, m2, m3 :: Enrollment
m1 = matricula "00000000-0000-0000-0000-0000000000e1" ana cursoHaskell Ativa
m2 = matricula "00000000-0000-0000-0000-0000000000e2" bruno cursoHaskell Ativa
m3 = matricula "00000000-0000-0000-0000-0000000000e3" bruno cursoSQL Trancada

-- Ana tira 9 em tudo (NF 9); Bruno tira 6 em tudo (NF 6). Ninguém tem nota em SQL.
notasExemplo :: [Grade]
notasExemplo =
    [ nota 9 9 (Just 9) |> set #enrollmentId m1.id
    , nota 6 6 (Just 6) |> set #enrollmentId m2.id
    ]

specs :: Spec
specs = do
    describe "validarCargaHoraria (Sprint 2)" do
        it "aceita carga horária positiva" do
            validarCargaHoraria 60 `shouldBe` Right 60
        it "rejeita zero ou valores negativos" do
            validarCargaHoraria 0 `shouldBe` Left (CargaHorariaInvalida 0)
            validarCargaHoraria (-5) `shouldBe` Left (CargaHorariaInvalida (-5))
        it "gera a mensagem em português usada no formulário" do
            map mensagemDeErro (lefts [validarCargaHoraria 0]) `shouldBe` ["A carga horária deve ser maior que zero"]
            mensagemDeErro (NotaForaDoIntervalo "pe1" 11) `shouldBe` "A nota deve estar entre 0 e 10"

    describe "relatórios (map, filter, fold, concatMap)" do
        it "mediaPorCurso calcula a média das NFs e devolve Nothing para curso sem notas" do
            mediaPorCurso [cursoHaskell, cursoSQL] [m1, m2, m3] notasExemplo
                `shouldBe` [("Haskell", Just 7.5), ("SQL", Nothing)]
        it "rankingDeAlunos ordena pela média, da maior para a menor" do
            rankingDeAlunos [bruno, ana] [m1, m2, m3] notasExemplo
                `shouldBe` [(1, "Ana", 9), (2, "Bruno", 6)]
        it "alunosPorCurso ignora matrículas trancadas e ordena os nomes" do
            alunosPorCurso [cursoHaskell, cursoSQL] [bruno, ana] [m1, m2, m3]
                `shouldBe` [("Haskell", ["Ana", "Bruno"]), ("SQL", [])]
        it "contarPorSituacao conta cada situação com foldl'" do
            contarPorSituacao [("Ana", 9, Aprovado), ("Bruno", 6, Aprovado), ("Caio", 3, Reprovado)]
                `shouldBe` [(Aprovado, 2), (AguardandoAV3, 0), (Reprovado, 1), (Trancado, 0)]
        it "cargaHorariaTotal soma a carga de todos os cursos" do
            cargaHorariaTotal [cursoHaskell, cursoSQL] `shouldBe` 100
            cargaHorariaTotal [] `shouldBe` 0

    describe "filtros do relatório (FiltroRelatorio, filtrarPorCurso, filtrarPorStatus)" do
        it "filtrarPorCurso filtra matrículas pelo Id do curso quando Just" do
            filtrarPorCurso (Just cursoHaskell.id) [m1, m2, m3] `shouldBe` [m1, m2]
            filtrarPorCurso (Just cursoSQL.id) [m1, m2, m3] `shouldBe` [m3]
        it "filtrarPorCurso mantém todas as matrículas quando Nothing" do
            filtrarPorCurso Nothing [m1, m2, m3] `shouldBe` [m1, m2, m3]
        it "filtrarPorStatus filtra pelo status quando Just" do
            filtrarPorStatus (Just Ativa) [m1, m2, m3] `shouldBe` [m1, m2]
            filtrarPorStatus (Just Trancada) [m1, m2, m3] `shouldBe` [m3]
            filtrarPorStatus (Just Concluida) [m1, m2, m3] `shouldBe` []
        it "filtrarPorStatus mantém todas as matrículas quando Nothing" do
            filtrarPorStatus Nothing [m1, m2, m3] `shouldBe` [m1, m2, m3]
        it "aplicarFiltro combina curso e status via record FiltroRelatorio" do
            let filtroHaskellAtiva = FiltroRelatorio (Just cursoHaskell.id) (Just Ativa)
            aplicarFiltro filtroHaskellAtiva [m1, m2, m3] `shouldBe` [m1, m2]

            let filtroHaskellTrancada = FiltroRelatorio (Just cursoHaskell.id) (Just Trancada)
            aplicarFiltro filtroHaskellTrancada [m1, m2, m3] `shouldBe` []

            let filtroSQLTrancada = FiltroRelatorio (Just cursoSQL.id) (Just Trancada)
            aplicarFiltro filtroSQLTrancada [m1, m2, m3] `shouldBe` [m3]

            let filtroVazio' = FiltroRelatorio Nothing Nothing
            aplicarFiltro filtroVazio' [m1, m2, m3] `shouldBe` [m1, m2, m3]
        it "agregações passam a receber a lista já filtrada (reuso das funções atuais)" do
            let matriculasHaskell = filtrarPorCurso (Just cursoHaskell.id) [m1, m2, m3]
            mediaPorCurso [cursoHaskell] matriculasHaskell notasExemplo
                `shouldBe` [("Haskell", Just 7.5)]
            rankingDeAlunos [ana, bruno] matriculasHaskell notasExemplo
                `shouldBe` [(1, "Ana", 9.0), (2, "Bruno", 6.0)]

            -- Com apenas SQL, Bruno tem matrícula trancada e sem notas
            let matriculasSQL = filtrarPorCurso (Just cursoSQL.id) [m1, m2, m3]
            rankingDeAlunos [ana, bruno] matriculasSQL notasExemplo `shouldBe` []

            -- Boletim com apenas matrículas ativas de Haskell
            let ativasHaskell = aplicarFiltro (FiltroRelatorio (Just cursoHaskell.id) (Just Ativa)) [m1, m2, m3]
            map (\(nome, media, sit) -> (nome, media, sit)) (boletim [ana, bruno] ativasHaskell notasExemplo)
                `shouldBe` [("Ana", 9.0, Aprovado), ("Bruno", 6.0, Aprovado)]
        it "taxaAprovacao calcula percentual ou Nothing se lista vazia" do
            let linhas = boletim [ana, bruno] [m1, m2] notasExemplo
            taxaAprovacao linhas `shouldBe` Just 100.0
            taxaAprovacao [] `shouldBe` Nothing
        it "totalMatriculasAtivas e totalAlunosFiltrados calculam KPIs puros" do
            totalMatriculasAtivas [m1, m2, m3] `shouldBe` 2
            totalMatriculasAtivas [m3] `shouldBe` 0
            totalAlunosFiltrados filtroVazio [ana, bruno] [m1, m2, m3] `shouldBe` 2
            totalAlunosFiltrados (FiltroRelatorio (Just cursoSQL.id) Nothing) [ana, bruno] [m3] `shouldBe` 1
        it "parseStatus faz parse case-insensitive ou devolve Nothing" do
            parseStatus "ativa" `shouldBe` Just Ativa
            parseStatus "Ativa" `shouldBe` Just Ativa
            parseStatus "TRANCADA" `shouldBe` Just Trancada
            parseStatus "invalido" `shouldBe` Nothing

    describe "podeAcessar (papéis e permissões)" do
        it "administrador pode tudo" do
            and [ podeAcessar Admin op r | op <- [minBound .. maxBound], r <- [minBound .. maxBound] ] `shouldBe` True
        it "só o administrador gerencia usuários" do
            map (\papel -> podeAcessar papel Visualizar Usuarios) [Admin, Coordenador, Professor, Aluno]
                `shouldBe` [True, False, False, False]
        it "coordenador vê as notas, mas não as altera" do
            (podeAcessar Coordenador Visualizar Notas, podeAcessar Coordenador Gerenciar Notas) `shouldBe` (True, False)
        it "professor lança notas, mas não altera cursos" do
            (podeAcessar Professor Gerenciar Notas, podeAcessar Professor Gerenciar Cursos) `shouldBe` (True, False)
        it "aluno só visualiza cursos e relatórios" do
            recursosVisiveis Aluno `shouldBe` [Cursos, Relatorios]
            podeAcessar Aluno Gerenciar Cursos `shouldBe` False

    describe "mediaPonderada (currying e closures)" do
        it "compõe a AV com 70% PE + 30% AVf" do
            comporAV 8 6 `shouldBe` 7.4
        it "mediaSimples é a mesma função com outros pesos" do
            mediaSimples 5 10 `shouldBe` 7.5
        it "a aplicação parcial pode ser guardada e reutilizada" do
            let pesoDobradoNaFormativa = mediaPonderada (Pesos 1 2)
            map (uncurry pesoDobradoNaFormativa) [(3, 6), (9, 0)] `shouldBe` [5, 3]

    describe "composição das notas" do
        let grade = newRecord @Grade
                |> set #pe1 8 |> set #avf1 6
                |> set #pe2 6 |> set #avf2 10
        it "calcula AV1, AV2 e a média parcial" do
            (notaAV1 grade, notaAV2 grade, mediaParcial grade) `shouldBe` (7.4, 7.2, 7.3)
        it "AV3 só existe quando PE3 e AVf3 foram lançadas" do
            notaAV3 grade `shouldBe` Nothing
            notaAV3 (grade |> set #pe3 (Just 5)) `shouldBe` Nothing
            notaAV3 (grade |> set #pe3 (Just 5) |> set #avf3 (Just 5)) `shouldBe` Just 5
        it "NF = (média parcial + AV3) / 2" do
            mediaFinal (nota 6 6 (Just 5)) `shouldBe` 5.5
            mediaFinal (nota 6 8 Nothing) `shouldBe` 7

    describe "classificarMedia (IMC adaptado)" do
        it "enquadra a média em faixas" do
            map classificarMedia [2, 5, 7, 8, 9.5]
                `shouldBe` [Insuficiente, Regular, Bom, Otimo, Excelente]

    describe "situacaoDaMatricula (pattern matching)" do
        it "reprova se a média de AV1 e AV2 for menor que 4" do
            situacaoDaMatricula Ativa (nota 3 3 Nothing) `shouldBe` Reprovado
        it "aguarda a AV3 enquanto ela não for lançada" do
            situacaoDaMatricula Ativa (nota 6 6 Nothing) `shouldBe` AguardandoAV3
        it "reprova se a AV3 for menor que 4" do
            situacaoDaMatricula Ativa (nota 6 6 (Just 3)) `shouldBe` Reprovado
        it "aprova com NF >= 5" do
            situacaoDaMatricula Ativa (nota 6 6 (Just 5)) `shouldBe` Aprovado
        it "reprova com NF < 5 mesmo com AV3 >= 4" do
            situacaoDaMatricula Ativa (nota 4 4 (Just 4)) `shouldBe` Reprovado
        it "matrícula trancada ignora as notas" do
            situacaoDaMatricula Trancada (nota 10 10 (Just 10)) `shouldBe` Trancado

    describe "validações" do
        it "aceita notas entre 0 e 10" do
            validarNota "pe1" 7 `shouldBe` Right 7
            validarNota "pe1" 11 `shouldBe` Left (NotaForaDoIntervalo "pe1" 11)
        it "valida todas as notas de uma Grade" do
            validarGrade (nota 7 7 Nothing |> set #pe2 (-1) |> set #pe3 (Just 12))
                `shouldBe` [NotaForaDoIntervalo "pe2" (-1), NotaForaDoIntervalo "pe3" 12]
        it "valida nome e e-mail do estudante" do
            let student = newRecord @Student |> set #name " " |> set #email "ana.com"
            validarStudent student `shouldBe` [CampoVazio "name", EmailInvalido "ana.com"]
            validarStudent (student |> set #name "Ana" |> set #email "ana@unifor.br")
                `shouldBe` []
        it "valida carga horária do curso" do
            let course = newRecord @Course |> set #name "Haskell" |> set #workload 0
            validarCourse course `shouldBe` [CargaHorariaInvalida 0]

    describe "coleções (list comprehensions)" do
        let ana    = newRecord @Student |> set #id "11111111-1111-1111-1111-111111111111" |> set #name "Ana"
            bruno  = newRecord @Student |> set #id "22222222-2222-2222-2222-222222222222" |> set #name "Bruno"
            curso  = newRecord @Course  |> set #id "33333333-3333-3333-3333-333333333333" |> set #name "Haskell"
            matAna   = newRecord @Enrollment
                |> set #id "44444444-4444-4444-4444-444444444444"
                |> set #studentId ana.id |> set #courseId curso.id |> set #status Ativa
            matBruno = newRecord @Enrollment
                |> set #id "55555555-5555-5555-5555-555555555555"
                |> set #studentId bruno.id |> set #courseId curso.id |> set #status Trancada
            notaAna   = nota 8 9 (Just 7) |> set #enrollmentId matAna.id
            notaBruno = nota 3 3 Nothing |> set #enrollmentId matBruno.id
            linhas = boletim [ana, bruno] [matAna, matBruno] [notaAna, notaBruno]

        it "monta o boletim cruzando alunos, matrículas e notas" do
            map (\(nome, _, situacao) -> (nome, situacao)) linhas
                `shouldBe` [("Ana", Aprovado), ("Bruno", Trancado)]
        it "filtra os aprovados" do
            nomesAprovados linhas `shouldBe` ["Ana"]
        it "conta matrículas ativas por curso" do
            matriculasAtivasPorCurso [curso] [matAna, matBruno] `shouldBe` [("Haskell", 1)]

    describe "mediaDaTurma (recursão em cauda)" do
        it "calcula a média" do
            mediaDaTurma [6, 8, 10] `shouldBe` Just 8
        it "turma vazia não tem média" do
            mediaDaTurma [] `shouldBe` Nothing

    describe "validarCadastro (Either + acúmulo de erros)" do
        let valido = DadosCadastro
                { nomeCadastro = "Nicolly"
                , emailCadastro = "nicolly@unifor.br"
                , senhaCadastro = "boaSenha9"
                , confirmacaoCadastro = "boaSenha9"
                }
        it "aceita um cadastro bem preenchido" do
            validarCadastro valido `shouldBe` Right valido
        it "acumula todos os erros de uma vez, na ordem das regras" do
            validarCadastro (valido { nomeCadastro = "  ", emailCadastro = "x", confirmacaoCadastro = "outra" })
                `shouldBe` Left [NomeObrigatorio, EmailInvalidoCadastro, SenhasNaoConferem]
        it "reporta só a senha quando o resto está certo" do
            validarCadastro (valido { confirmacaoCadastro = "diferente" })
                `shouldBe` Left [SenhasNaoConferem]
        it "gera as mensagens em português" do
            map mensagemDoCadastro [NomeObrigatorio, EmailInvalidoCadastro, SenhasNaoConferem]
                `shouldBe` ["Informe o seu nome", "Informe um e-mail válido", "A senha e a confirmação não conferem"]

    describe "pipelineCadastroCPS (Continuation Passing Style)" do
        let dados = DadosCadastro "Nicolly" "nicolly@unifor.br" "boaSenha9" "boaSenha9"
            -- continuações simples que marcam qual caminho foi seguido
            executa d forca email = pipelineCadastroCPS d forca email ("erro: " <>) (const "ok")
        it "segue para o sucesso quando tudo passa" do
            executa dados (Right ()) Nothing `shouldBe` "ok"
        it "aborta no primeiro erro de campo via continuação de erro" do
            executa (dados { nomeCadastro = "" }) (Right ()) Nothing `shouldBe` "erro: Informe o seu nome"
        it "aborta pela força da senha antes de olhar o e-mail" do
            executa dados (Left "senha fraca") (Just "em uso") `shouldBe` "erro: senha fraca"
        it "aborta quando o e-mail já está em uso" do
            executa dados (Right ()) (Just "Este e-mail já está em uso. Que tal nicolly1@unifor.br?")
                `shouldBe` "erro: Este e-mail já está em uso. Que tal nicolly1@unifor.br?"

    describe "pipelineMatriculaCPS (Continuation Passing Style no fluxo de matrícula)" do
        let novaMatricula = matricula "00000000-0000-0000-0000-0000000000e4" ana cursoSQL Ativa
            executa m ss cs es = pipelineMatriculaCPS m ss cs es (\err -> "erro: " <> mensagemErroMatricula err) (const "ok")

        it "invoca onSucesso quando aluno e curso existem e não há duplicidade" do
            executa novaMatricula [ana, bruno] [cursoHaskell, cursoSQL] [m1, m2] `shouldBe` "ok"

        it "aborta com AlunoNaoEncontrado quando o aluno não está cadastrado" do
            let alunoInexistente = newRecord @Student |> set #id "00000000-0000-0000-0000-000000000099"
                mInvalida = matricula "00000000-0000-0000-0000-0000000000e4" alunoInexistente cursoSQL Ativa
            executa mInvalida [ana, bruno] [cursoHaskell, cursoSQL] []
                `shouldBe` "erro: Selecione um aluno válido"

        it "aborta com CursoNaoEncontrado quando o curso não está cadastrado" do
            let cursoInexistente = newRecord @Course |> set #id "00000000-0000-0000-0000-000000000099"
                mInvalida = matricula "00000000-0000-0000-0000-0000000000e4" ana cursoInexistente Ativa
            executa mInvalida [ana, bruno] [cursoHaskell, cursoSQL] []
                `shouldBe` "erro: Selecione um curso válido"

        it "aborta com MatriculaDuplicada quando o aluno já possui matrícula no curso" do
            -- m1 é matrícula de Ana no cursoHaskell
            let mDuplicada = matricula "00000000-0000-0000-0000-0000000000e5" ana cursoHaskell Ativa
            executa mDuplicada [ana, bruno] [cursoHaskell, cursoSQL] [m1]
                `shouldBe` "erro: Este aluno já está matriculado neste curso"

        it "permite validação em CPS retornando Either diretamente via continuações" do
            pipelineMatriculaCPS novaMatricula [ana, bruno] [cursoHaskell, cursoSQL] [m1, m2] Left Right
                `shouldBe` Right novaMatricula

    describe "avaliação preguiçosa (variacoesDeEmail / primeiroEmailLivre)" do
        it "gera variações sufixadas do e-mail" do
            take 3 (variacoesDeEmail "ana@unifor.br")
                `shouldBe` ["ana@unifor.br", "ana1@unifor.br", "ana2@unifor.br"]
        it "acha a primeira variação livre percorrendo uma lista infinita" do
            primeiroEmailLivre ["ana@unifor.br", "ana1@unifor.br"] "ana@unifor.br"
                `shouldBe` "ana2@unifor.br"
        it "devolve o próprio e-mail quando ele já está livre" do
            primeiroEmailLivre ["outro@unifor.br"] "ana@unifor.br" `shouldBe` "ana@unifor.br"

    -- -----------------------------------------------------------------------
    -- Sprint 5: tratamento de erros monádico (Application.Errors)
    -- -----------------------------------------------------------------------
    describe "classificarFalhaSql (tradução de exceções do PostgreSQL)" do
        it "unique_violation (23505) vira Conflito" do
            classificarFalhaSql "ERROR: duplicate key value violates unique constraint \"users_email_key\" (23505)"
                `shouldSatisfy` \case Conflito _ -> True; _ -> False
        it "foreign_key_violation (23503) vira ErroDeValidacao" do
            classificarFalhaSql "insert or update violates foreign key constraint (SQLSTATE 23503)"
                `shouldSatisfy` \case ErroDeValidacao _ -> True; _ -> False
        it "check_violation (23514) vira ErroDeValidacao" do
            classificarFalhaSql "new row violates check constraint \"grades_pe1_check\" 23514"
                `shouldSatisfy` \case ErroDeValidacao _ -> True; _ -> False
        it "qualquer outra falha vira FalhaDeInfra preservando o detalhe para o log" do
            classificarFalhaSql "connection refused" `shouldBe` FalhaDeInfra "connection refused"

    describe "mensagemAppError / codigoHttp" do
        it "nunca expõe o detalhe técnico ao usuário" do
            mensagemAppError (FalhaDeInfra "senha do banco: xyz") `shouldNotSatisfy` Text.isInfixOf "xyz"
        it "junta as mensagens de validação" do
            mensagemAppError (ErroDeValidacao ["A.", "B."]) `shouldBe` "A. B."
        it "mapeia cada erro para o status HTTP adequado" do
            map codigoHttp [ErroDeValidacao [], NaoEncontrado "x", Conflito "x", NaoAutenticado, SemPermissao, FalhaDeInfra "x"]
                `shouldBe` [422, 404, 409, 401, 403, 503]
        it "traduz erros específicos dos módulos para AppError" do
            deErroMatricula MatriculaDuplicada `shouldBe` ErroDeValidacao [mensagemErroMatricula MatriculaDuplicada]
            deErroSenha SenhaSemNumero `shouldBe` ErroDeValidacao [mensagemDaSenha SenhaSemNumero]

    describe "AppM (ExceptT AppError IO)" do
        it "encadeia etapas e devolve Right quando todas passam" do
            resultado <- executarApp do
                a <- exigirJust (NaoEncontrado "A") (Just (2 :: Int))
                b <- deEither deErroSenha (Right 3)
                pure (a + b)
            resultado `shouldBe` Right 5
        it "interrompe na primeira falha (curto-circuito) sem executar o resto" do
            resultado <- executarApp do
                _ <- exigirJust (NaoEncontrado "Aluno") (Nothing :: Maybe Int)
                _ <- capturarInfra "teste" (error "não deveria executar" :: IO Int)
                pure ()
            resultado `shouldBe` Left (NaoEncontrado "Aluno")
        it "capturarInfra transforma exceção de IO em Left (sem derrubar o programa)" do
            resultado <- executarApp (capturarInfra "teste" (ioError (userError "banco fora do ar") :: IO Int))
            resultado `shouldSatisfy` \case Left (FalhaDeInfra _) -> True; _ -> False
        it "deEither eleva a validação pura do pipeline CPS para o fluxo" do
            -- outra matrícula (id diferente) da Ana no mesmo curso de m1
            let duplicada = matricula "00000000-0000-0000-0000-0000000000e9" ana cursoHaskell Ativa
            resultado <- executarApp do
                deEither deErroMatricula
                    (pipelineMatriculaCPS duplicada [ana] [cursoHaskell] [m1] Left Right)
            resultado `shouldBe` Left (deErroMatricula MatriculaDuplicada)

    -- -----------------------------------------------------------------------
    -- Segurança (Application.Security): Reader, bcrypt e AES-256-GCM
    -- -----------------------------------------------------------------------
    describe "validarForcaSenha (Reader AuthConfig)" do
        it "usa o tamanho mínimo vindo da configuração" do
            avaliarForcaSenha configPadrao { minPasswordLength = 12 } "abc12345"
                `shouldBe` Left (SenhaMuitoCurta 12)
        it "exige número e letra" do
            avaliarForcaSenha configPadrao "abcdefgh" `shouldBe` Left SenhaSemNumero
            avaliarForcaSenha configPadrao "12345678" `shouldBe` Left SenhaSemLetra
            avaliarForcaSenha configPadrao "abcd1234" `shouldBe` Right ()

    describe "hash de senha (bcrypt)" do
        let configRapida = configPadrao { bcryptCost = 4 } -- custo baixo só para o teste ser rápido
        it "gera hash diferente da senha e que confere com ela" do
            hash <- gerarHashSenha configRapida "senha123"
            hash `shouldNotBe` "senha123"
            conferirSenha "senha123" hash `shouldBe` True
            conferirSenha "senhaErrada1" hash `shouldBe` False
        it "usa salt: a mesma senha gera hashes diferentes" do
            h1 <- gerarHashSenha configRapida "senha123"
            h2 <- gerarHashSenha configRapida "senha123"
            h1 `shouldNotBe` h2

    describe "criptografia de campo (AES-256-GCM)" do
        it "cifra e decifra de volta (ida e volta)" do
            blob <- cifrarCampo configPadrao "123.456.789-00"
            blob `shouldNotBe` "123.456.789-00"
            decifrarCampo configPadrao blob `shouldBe` Just "123.456.789-00"
        it "nonce aleatório: o mesmo texto gera cifras diferentes" do
            b1 <- cifrarCampo configPadrao "123.456.789-00"
            b2 <- cifrarCampo configPadrao "123.456.789-00"
            b1 `shouldNotBe` b2
        it "com a chave errada não decifra (Nothing, sem exceção)" do
            blob <- cifrarCampo configPadrao "segredo"
            let outraChave = configPadrao { encryptionKey = "0123456789abcdef0123456789abcdef" }
            decifrarCampo outraChave blob `shouldBe` Nothing
        it "detecta adulteração e entrada inválida" do
            decifrarCampo configPadrao "isto-nao-e-base64!!" `shouldBe` Nothing
            decifrarCampo configPadrao "" `shouldBe` Nothing
        it "rejeita cifra adulterada (um caractere trocado) e truncada" do
            blob <- cifrarCampo configPadrao "123.456.789-00"
            let trocar c = if c == 'A' then 'B' else 'A'
                adulterado = Text.take 20 blob <> Text.singleton (trocar (Text.index blob 20)) <> Text.drop 21 blob
            decifrarCampo configPadrao adulterado `shouldBe` Nothing
            decifrarCampo configPadrao (Text.take 16 blob) `shouldBe` Nothing
        it "campo opcional em branco não é cifrado" do
            vazio <- cifrarOpcional configPadrao "   "
            vazio `shouldBe` Nothing
