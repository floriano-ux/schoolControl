module Main where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Domain
import Application.Security
import Application.Security.GeradorSenha
import Application.UseCase.CadastrarAluno
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
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

aluno :: Id User -> Text -> User
aluno userId nome = newRecord @User |> set #id userId |> set #name nome |> set #userRole Aluno

ana, bruno, professora :: User
ana        = aluno "00000000-0000-0000-0000-0000000000a1" "Ana"
bruno      = aluno "00000000-0000-0000-0000-0000000000b1" "Bruno"
professora = aluno "00000000-0000-0000-0000-0000000000f1" "Paula" |> set #userRole Professor

disciplina :: Id Subject -> Text -> Course -> Subject
disciplina subjectId nome course = newRecord @Subject
    |> set #id subjectId |> set #name nome |> set #courseId course.id |> set #workload 30

funcional, monadas, consultas :: Subject
funcional = disciplina "00000000-0000-0000-0000-0000000000d1" "Funcional" cursoHaskell |> set #professorId (Just professora.id)
monadas   = disciplina "00000000-0000-0000-0000-0000000000d2" "Mônadas" cursoHaskell
consultas = disciplina "00000000-0000-0000-0000-0000000000d3" "Consultas" cursoSQL

usuarios :: [User]
usuarios = [ana, bruno, professora]

disciplinas :: [Subject]
disciplinas = [funcional, monadas, consultas]

cursos :: [Course]
cursos = [cursoHaskell, cursoSQL]

matricula :: Id Enrollment -> User -> Subject -> EnrollmentStatus -> Enrollment
matricula matriculaId user subject status = newRecord @Enrollment
    |> set #id matriculaId |> set #studentId user.id |> set #subjectId subject.id |> set #status status

m1, m2, m3 :: Enrollment
m1 = matricula "00000000-0000-0000-0000-0000000000e1" ana funcional Ativa
m2 = matricula "00000000-0000-0000-0000-0000000000e2" bruno funcional Ativa
m3 = matricula "00000000-0000-0000-0000-0000000000e3" bruno consultas Trancada

detalhadas :: [MatriculaDetalhada]
detalhadas = detalharMatriculas usuarios disciplinas cursos [m1, m2, m3]

matriculasDe :: [MatriculaDetalhada] -> [Enrollment]
matriculasDe = map (.matricula)

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
        it "detalharMatriculas cruza matrícula, aluno, disciplina e curso" do
            map (\m -> (m.aluno.name, m.disciplina.name, m.curso.name)) detalhadas
                `shouldBe` [("Ana", "Funcional", "Haskell"), ("Bruno", "Funcional", "Haskell"), ("Bruno", "Consultas", "SQL")]
        it "detalharMatriculas descarta matrículas sem disciplina correspondente" do
            detalharMatriculas usuarios [funcional] cursos [m1, m3] `shouldSatisfy` ((== 1) . length)
        it "mediaPorCurso calcula a média das NFs e devolve Nothing para curso sem notas" do
            mediaPorCurso [cursoHaskell, cursoSQL] detalhadas notasExemplo
                `shouldBe` [("Haskell", Just 7.5), ("SQL", Nothing)]
        it "mediaPorDisciplina rotula como \"Disciplina (Curso)\" e devolve Nothing sem notas" do
            mediaPorDisciplina (disciplinasComCurso cursos disciplinas) detalhadas notasExemplo
                `shouldBe` [("Funcional (Haskell)", Just 7.5), ("Mônadas (Haskell)", Nothing), ("Consultas (SQL)", Nothing)]
        it "rankingDeAlunos ordena pela média, da maior para a menor" do
            rankingDeAlunos (reverse detalhadas) notasExemplo
                `shouldBe` [(1, "Ana", 9), (2, "Bruno", 6)]
        it "alunosPorCurso ignora matrículas trancadas e ordena os nomes" do
            alunosPorCurso [cursoHaskell, cursoSQL] detalhadas
                `shouldBe` [("Haskell", ["Ana", "Bruno"]), ("SQL", [])]
        it "alunosPorCurso não repete o aluno matriculado em duas disciplinas do curso" do
            let extra = matricula "00000000-0000-0000-0000-0000000000e9" ana monadas Ativa
            alunosPorCurso [cursoHaskell] (detalharMatriculas usuarios disciplinas cursos [m1, m2, extra])
                `shouldBe` [("Haskell", ["Ana", "Bruno"])]
        it "contarPorSituacao conta cada situação com foldl'" do
            contarPorSituacao [("Ana", 9, Aprovado), ("Bruno", 6, Aprovado), ("Caio", 3, Reprovado)]
                `shouldBe` [(Aprovado, 2), (AguardandoAV3, 0), (Reprovado, 1), (Trancado, 0)]
        it "cargaHorariaTotal soma a carga de todos os cursos" do
            cargaHorariaTotal [cursoHaskell, cursoSQL] `shouldBe` 100
            cargaHorariaTotal [] `shouldBe` 0

    describe "filtros do relatório (FiltroRelatorio, filtrarPorCurso, filtrarPorDisciplina, filtrarPorStatus)" do
        it "filtrarPorCurso filtra matrículas pelo Id do curso quando Just" do
            matriculasDe (filtrarPorCurso (Just cursoHaskell.id) detalhadas) `shouldBe` [m1, m2]
            matriculasDe (filtrarPorCurso (Just cursoSQL.id) detalhadas) `shouldBe` [m3]
        it "filtrarPorCurso mantém todas as matrículas quando Nothing" do
            matriculasDe (filtrarPorCurso Nothing detalhadas) `shouldBe` [m1, m2, m3]
        it "filtrarPorDisciplina filtra matrículas pelo Id da disciplina" do
            matriculasDe (filtrarPorDisciplina (Just consultas.id) detalhadas) `shouldBe` [m3]
            matriculasDe (filtrarPorDisciplina (Just monadas.id) detalhadas) `shouldBe` []
            matriculasDe (filtrarPorDisciplina Nothing detalhadas) `shouldBe` [m1, m2, m3]
        it "filtrarPorStatus filtra pelo status quando Just" do
            matriculasDe (filtrarPorStatus (Just Ativa) detalhadas) `shouldBe` [m1, m2]
            matriculasDe (filtrarPorStatus (Just Trancada) detalhadas) `shouldBe` [m3]
            matriculasDe (filtrarPorStatus (Just Concluida) detalhadas) `shouldBe` []
        it "filtrarPorStatus mantém todas as matrículas quando Nothing" do
            matriculasDe (filtrarPorStatus Nothing detalhadas) `shouldBe` [m1, m2, m3]
        it "aplicarFiltro combina curso, disciplina e status via record FiltroRelatorio" do
            let filtrar cursoId disciplinaId status = matriculasDe (aplicarFiltro (FiltroRelatorio cursoId disciplinaId status) detalhadas)
            filtrar (Just cursoHaskell.id) Nothing (Just Ativa) `shouldBe` [m1, m2]
            filtrar (Just cursoHaskell.id) Nothing (Just Trancada) `shouldBe` []
            filtrar (Just cursoSQL.id) Nothing (Just Trancada) `shouldBe` [m3]
            filtrar Nothing (Just funcional.id) Nothing `shouldBe` [m1, m2]
            filtrar (Just cursoSQL.id) (Just funcional.id) Nothing `shouldBe` []
            filtrar Nothing Nothing Nothing `shouldBe` [m1, m2, m3]
        it "filtrarDisciplinas respeita os filtros de curso e de disciplina" do
            let pares = disciplinasComCurso cursos disciplinas
                nomes filtro = map (\(s, _) -> s.name) (filtrarDisciplinas filtro pares)
            nomes filtroVazio `shouldBe` ["Funcional", "Mônadas", "Consultas"]
            nomes filtroVazio { cursoId = Just cursoHaskell.id } `shouldBe` ["Funcional", "Mônadas"]
            nomes filtroVazio { disciplinaId = Just consultas.id } `shouldBe` ["Consultas"]
        it "agregações passam a receber a lista já filtrada (reuso das funções atuais)" do
            let matriculasHaskell = filtrarPorCurso (Just cursoHaskell.id) detalhadas
            mediaPorCurso [cursoHaskell] matriculasHaskell notasExemplo
                `shouldBe` [("Haskell", Just 7.5)]
            rankingDeAlunos matriculasHaskell notasExemplo
                `shouldBe` [(1, "Ana", 9.0), (2, "Bruno", 6.0)]

            -- Com apenas SQL, Bruno tem matrícula trancada e sem notas
            let matriculasSQL = filtrarPorCurso (Just cursoSQL.id) detalhadas
            rankingDeAlunos matriculasSQL notasExemplo `shouldBe` []

            -- Boletim com apenas matrículas ativas de Haskell
            let ativasHaskell = aplicarFiltro (FiltroRelatorio (Just cursoHaskell.id) Nothing (Just Ativa)) detalhadas
            boletim ativasHaskell notasExemplo
                `shouldBe` [("Ana", 9.0, Aprovado), ("Bruno", 6.0, Aprovado)]
        it "boletimDoAluno junta cada matrícula à sua nota e ordena por curso e disciplina" do
            let doBruno = filter (\m -> m.aluno.id == bruno.id) detalhadas
                linhas  = boletimDoAluno doBruno notasExemplo
            map (\(m, g) -> (m.disciplina.name, fmap mediaFinal g)) linhas
                `shouldBe` [("Funcional", Just 6), ("Consultas", Nothing)]
            mediaGeral linhas `shouldBe` Just 6
            mediaGeral [] `shouldBe` Nothing
        it "taxaAprovacao calcula percentual ou Nothing se lista vazia" do
            let linhas = boletim (take 2 detalhadas) notasExemplo
            taxaAprovacao linhas `shouldBe` Just 100.0
            taxaAprovacao [] `shouldBe` Nothing
        it "totalMatriculasAtivas e totalAlunosFiltrados calculam KPIs puros" do
            totalMatriculasAtivas detalhadas `shouldBe` 2
            totalMatriculasAtivas (drop 2 detalhadas) `shouldBe` 0
            totalAlunosFiltrados filtroVazio (alunos usuarios) detalhadas `shouldBe` 2
            let filtroSQL = filtroVazio { cursoId = Just cursoSQL.id }
            totalAlunosFiltrados filtroSQL (alunos usuarios) (aplicarFiltro filtroSQL detalhadas) `shouldBe` 1
        it "parseStatus faz parse case-insensitive ou devolve Nothing" do
            parseStatus "ativa" `shouldBe` Just Ativa
            parseStatus "Ativa" `shouldBe` Just Ativa
            parseStatus "TRANCADA" `shouldBe` Just Trancada
            parseStatus "invalido" `shouldBe` Nothing

    describe "podeAcessar (papéis e permissões)" do
        it "administrador pode tudo, exceto o boletim pessoal de aluno e lançar notas" do
            and [ podeAcessar Admin op r | op <- [minBound .. maxBound], r <- [minBound .. maxBound], r `notElem` [Boletim, Notas] ] `shouldBe` True
            podeAcessar Admin Visualizar Boletim `shouldBe` False
        it "só o professor lança, edita e exclui notas" do
            map (\papel -> podeAcessar papel Gerenciar Notas) [Admin, Coordenador, Professor, Aluno]
                `shouldBe` [False, False, True, False]
            podeAcessar Admin Visualizar Notas `shouldBe` True
        it "só o administrador gerencia usuários" do
            map (\papel -> podeAcessar papel Visualizar Usuarios) [Admin, Coordenador, Professor, Aluno]
                `shouldBe` [True, False, False, False]
        it "coordenador vê as notas, mas não as altera" do
            (podeAcessar Coordenador Visualizar Notas, podeAcessar Coordenador Gerenciar Notas) `shouldBe` (True, False)
        it "professor não acessa a tela de matrículas" do
            (podeAcessar Professor Visualizar Matriculas, podeAcessar Professor Gerenciar Matriculas) `shouldBe` (False, False)
        it "professor lança notas, mas não altera cursos" do
            (podeAcessar Professor Gerenciar Notas, podeAcessar Professor Gerenciar Cursos) `shouldBe` (True, False)
        it "aluno só visualiza cursos, disciplinas e o próprio boletim" do
            recursosVisiveis Aluno `shouldBe` [Cursos, Disciplinas, Boletim]
            podeAcessar Aluno Visualizar Relatorios `shouldBe` False
            podeAcessar Aluno Gerenciar Cursos `shouldBe` False
            podeAcessar Aluno Gerenciar Disciplinas `shouldBe` False
        it "menu de cada papel da equipe" do
            recursosVisiveis Admin `shouldBe` [Estudantes, Cursos, Disciplinas, Matriculas, Notas, Relatorios, Usuarios]
            recursosVisiveis Coordenador `shouldBe` [Estudantes, Cursos, Disciplinas, Matriculas, Notas, Relatorios]
            recursosVisiveis Professor `shouldBe` [Estudantes, Cursos, Disciplinas, Notas, Relatorios]
        it "disciplinas seguem as mesmas regras de cursos" do
            [ podeAcessar papel op Disciplinas | papel <- [Admin, Coordenador, Professor, Aluno], op <- [minBound .. maxBound] ]
                `shouldBe` [ podeAcessar papel op Cursos | papel <- [Admin, Coordenador, Professor, Aluno], op <- [minBound .. maxBound] ]

    describe "papel de aluno (ehAluno, alunos, podeTrocarPapel)" do
        it "ehAluno e alunos filtram pelo papel" do
            map ehAluno usuarios `shouldBe` [True, True, False]
            map (.name) (alunos usuarios) `shouldBe` ["Ana", "Bruno"]
        it "aluno com matrículas não pode trocar de papel" do
            podeTrocarPapel Aluno Professor 2 `shouldBe` False
        it "aluno sem matrículas, ou sem mudar o papel, pode ser atualizado" do
            podeTrocarPapel Aluno Professor 0 `shouldBe` True
            podeTrocarPapel Aluno Aluno 3 `shouldBe` True
        it "professor que ministra disciplinas não pode trocar de papel" do
            podeTrocarPapel Professor Admin 2 `shouldBe` False
            podeTrocarPapel Professor Admin 0 `shouldBe` True
            podeTrocarPapel Coordenador Admin 5 `shouldBe` True
            mensagemTrocaDePapel Professor `shouldSatisfy` ("ministra disciplinas" `Text.isInfixOf`)
        it "o menu chama Estudantes de \"Meus alunos\" para o professor" do
            nomeDoRecurso Professor Estudantes `shouldBe` "Meus alunos"
            nomeDoRecurso Admin Estudantes `shouldBe` "Estudantes"

    describe "escopo do professor (Escopo)" do
        let outroProfessor = aluno "00000000-0000-0000-0000-0000000000f2" "Otávio" |> set #userRole Professor
        it "ministra apenas as disciplinas atribuídas a ele" do
            map (ministra professora) disciplinas `shouldBe` [True, False, False]
            map (.name) (disciplinasDoProfessor professora disciplinas) `shouldBe` ["Funcional"]
            ministra (professora |> set #userRole Admin) funcional `shouldBe` False
        it "disciplinas e cursos visíveis: professor vê os dele, os demais veem tudo" do
            map (.name) (disciplinasVisiveis professora disciplinas) `shouldBe` ["Funcional"]
            map (.name) (disciplinasVisiveis outroProfessor disciplinas) `shouldBe` []
            map (.name) (cursosVisiveis professora disciplinas cursos) `shouldBe` ["Haskell"]
            length (disciplinasVisiveis (aluno "00000000-0000-0000-0000-0000000000f3" "Admin" |> set #userRole Admin) disciplinas) `shouldBe` 3
        it "matrículas visíveis: professor só as das suas disciplinas; aluno só as próprias" do
            matriculasDe (matriculasVisiveis professora detalhadas) `shouldBe` [m1, m2]
            matriculasDe (matriculasVisiveis outroProfessor detalhadas) `shouldBe` []
            matriculasDe (matriculasVisiveis bruno detalhadas) `shouldBe` [m2, m3]
            map (matriculaVisivel professora) detalhadas `shouldBe` [True, True, False]
        it "alunos visíveis ao professor são os matriculados nas suas disciplinas" do
            map (.name) (alunosVisiveis professora (matriculasVisiveis professora detalhadas) [ana, bruno]) `shouldBe` ["Ana", "Bruno"]
            alunosVisiveis outroProfessor (matriculasVisiveis outroProfessor detalhadas) [ana, bruno] `shouldBe` []
        it "professorValido aceita vazio ou um usuário com papel de professor" do
            professorValido usuarios Nothing `shouldBe` True
            professorValido usuarios (Just professora.id) `shouldBe` True
            professorValido usuarios (Just ana.id) `shouldBe` False
        it "alunosComDisciplinas lista as disciplinas de cada aluno e filtra por disciplina" do
            let visiveis = matriculasVisiveis professora detalhadas
                nomes linhas = map (\(u, ds) -> (u.name, ds)) linhas
            nomes (alunosComDisciplinas Nothing detalhadas [ana, bruno])
                `shouldBe` [("Ana", ["Funcional"]), ("Bruno", ["Consultas", "Funcional"])]
            nomes (alunosComDisciplinas (Just consultas.id) detalhadas [ana, bruno])
                `shouldBe` [("Bruno", ["Consultas", "Funcional"])]
            nomes (alunosComDisciplinas Nothing visiveis [ana, bruno])
                `shouldBe` [("Ana", ["Funcional"]), ("Bruno", ["Funcional"])]
            alunosComDisciplinas (Just consultas.id) visiveis [ana, bruno] `shouldBe` []
        it "um professor pode ministrar várias disciplinas (redistribuirDisciplinas)" do
            let alteradas = redistribuirDisciplinas professora.id [funcional.id, monadas.id, consultas.id] disciplinas
                depois    = map (\s -> fromMaybe s (find (\a -> a.id == s.id) alteradas)) disciplinas
            map (.name) alteradas `shouldBe` ["Mônadas", "Consultas"]
            map (.name) (disciplinasDoProfessor professora depois) `shouldBe` ["Funcional", "Mônadas", "Consultas"]
        it "desmarcar uma disciplina a deixa sem professor; as de outros professores não mudam" do
            let outro     = aluno "00000000-0000-0000-0000-0000000000f2" "Otávio" |> set #userRole Professor
                deOutro   = monadas |> set #professorId (Just outro.id)
                alteradas = redistribuirDisciplinas professora.id [consultas.id] [funcional, deOutro, consultas]
            map (\s -> (s.name, s.professorId)) alteradas
                `shouldBe` [("Funcional", Nothing), ("Consultas", Just professora.id)]
            redistribuirDisciplinas professora.id [funcional.id] disciplinas `shouldBe` []
        it "nomeDoProfessor mostra o responsável ou \"Sem professor\"" do
            nomeDoProfessor usuarios funcional `shouldBe` "Paula"
            nomeDoProfessor usuarios consultas `shouldBe` "Sem professor"

    describe "lançamento de notas por disciplina" do
        it "matriculasDaDisciplina traz os alunos da disciplina em ordem alfabética" do
            map (\m -> m.aluno.name) (matriculasDaDisciplina funcional.id (reverse detalhadas)) `shouldBe` ["Ana", "Bruno"]
            matriculasDaDisciplina monadas.id detalhadas `shouldBe` []
        it "semNotaLancada deixa só os alunos ainda sem notas" do
            let soAnaComNota = take 1 notasExemplo
            map (\m -> m.aluno.name) (semNotaLancada soAnaComNota (matriculasDaDisciplina funcional.id detalhadas)) `shouldBe` ["Bruno"]
            semNotaLancada notasExemplo (matriculasDaDisciplina funcional.id detalhadas) `shouldBe` []

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
        it "valida carga horária do curso" do
            let course = newRecord @Course |> set #name "Haskell" |> set #workload 0
            validarCourse course `shouldBe` [CargaHorariaInvalida 0]
        it "valida nome e carga horária da disciplina" do
            let subject = newRecord @Subject |> set #name " " |> set #workload 0
            validarSubject subject `shouldBe` [CampoVazio "name", CargaHorariaInvalida 0]
            validarSubject (subject |> set #name "Mônadas" |> set #workload 20) `shouldBe` []

    describe "coleções (list comprehensions)" do
        let curso    = newRecord @Course |> set #id "33333333-3333-3333-3333-333333333333" |> set #name "Haskell"
            subject  = disciplina "66666666-6666-6666-6666-666666666666" "Funcional" curso
            matAna   = matricula "44444444-4444-4444-4444-444444444444" ana subject Ativa
            matBruno = matricula "55555555-5555-5555-5555-555555555555" bruno subject Trancada
            notaAna   = nota 8 9 (Just 7) |> set #enrollmentId matAna.id
            notaBruno = nota 3 3 Nothing |> set #enrollmentId matBruno.id
            turma  = detalharMatriculas [ana, bruno] [subject] [curso] [matAna, matBruno]
            linhas = boletim turma [notaAna, notaBruno]

        it "monta o boletim cruzando alunos, matrículas e notas" do
            map (\(nome, _, situacao) -> (nome, situacao)) linhas
                `shouldBe` [("Ana", Aprovado), ("Bruno", Trancado)]
        it "filtra os aprovados" do
            nomesAprovados linhas `shouldBe` ["Ana"]
        it "conta matrículas ativas por curso" do
            matriculasAtivasPorCurso [curso] turma `shouldBe` [("Haskell", 1)]

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
        let novaMatricula = matricula "00000000-0000-0000-0000-0000000000e4" ana consultas Ativa
            executa m es = pipelineMatriculaCPS m usuarios disciplinas es (\err -> "erro: " <> mensagemErroMatricula err) (const "ok")

        it "invoca onSucesso quando aluno e disciplina existem e não há duplicidade" do
            executa novaMatricula [m1, m2] `shouldBe` "ok"

        it "aborta com AlunoNaoEncontrado quando o aluno não está cadastrado" do
            let inexistente = aluno "00000000-0000-0000-0000-000000000099" "Fantasma"
            executa (matricula "00000000-0000-0000-0000-0000000000e4" inexistente consultas Ativa) []
                `shouldBe` "erro: Selecione um aluno válido"

        it "aborta com UsuarioNaoEhAluno quando o usuário tem outro papel" do
            executa (matricula "00000000-0000-0000-0000-0000000000e4" professora consultas Ativa) []
                `shouldBe` "erro: O usuário selecionado não é um aluno"

        it "aborta com DisciplinaNaoEncontrada quando a disciplina não está cadastrada" do
            let inexistente = disciplina "00000000-0000-0000-0000-000000000099" "Fantasma" cursoSQL
            executa (matricula "00000000-0000-0000-0000-0000000000e4" ana inexistente Ativa) []
                `shouldBe` "erro: Selecione uma disciplina válida"

        it "aborta com MatriculaDuplicada quando o aluno já possui matrícula na disciplina" do
            let mDuplicada = matricula "00000000-0000-0000-0000-0000000000e5" ana funcional Ativa
            executa mDuplicada [m1] `shouldBe` "erro: Este aluno já está matriculado nesta disciplina"

        it "permite matrícula do mesmo aluno em outra disciplina do mesmo curso" do
            executa (matricula "00000000-0000-0000-0000-0000000000e6" ana monadas Ativa) [m1] `shouldBe` "ok"

        it "permite validação em CPS retornando Either diretamente via continuações" do
            pipelineMatriculaCPS novaMatricula usuarios disciplinas [m1, m2] Left Right
                `shouldBe` Right novaMatricula
            pipelineMatriculaCPS (novaMatricula |> set #studentId professora.id) usuarios disciplinas [] Left Right
                `shouldBe` (Left UsuarioNaoEhAluno :: Either ErroMatricula Enrollment)

    describe "avaliação preguiçosa (variacoesDeEmail / primeiroEmailLivre)" do
        it "gera variações sufixadas do e-mail" do
            take 3 (variacoesDeEmail "ana@unifor.br")
                `shouldBe` ["ana@unifor.br", "ana1@unifor.br", "ana2@unifor.br"]
        it "acha a primeira variação livre percorrendo uma lista infinita" do
            primeiroEmailLivre ["ana@unifor.br", "ana1@unifor.br"] "ana@unifor.br"
                `shouldBe` "ana2@unifor.br"
        it "devolve o próprio e-mail quando ele já está livre" do
            primeiroEmailLivre ["outro@unifor.br"] "ana@unifor.br" `shouldBe` "ana@unifor.br"

    describe "validarForcaSenha (política de senha)" do
        it "exige ao menos uma letra maiúscula" do
            avaliarForcaSenha configPadrao "abcdefg1" `shouldBe` Left SenhaSemMaiuscula
        it "aceita senha com maiúscula, minúscula e número" do
            avaliarForcaSenha configPadrao "Abcdefg1" `shouldBe` Right ()
        it "mantém as regras de tamanho e de número" do
            avaliarForcaSenha configPadrao "Abc1" `shouldBe` Left (SenhaMuitoCurta 8)
            avaliarForcaSenha configPadrao "Abcdefgh" `shouldBe` Left SenhaSemNumero

    describe "montarSenha (gerador de senha puro)" do
        let fontes =
                [ take 200 (cycle [0 .. 255])
                , replicate 200 7
                , take 200 (cycle [255, 254 .. 0])
                , take 200 (cycle [13, 101, 200, 3, 77])
                ]
        it "gera senhas que respeitam a política de senha" do
            forM_ fontes \bytes -> do
                let resultado = montarSenha 12 bytes
                resultado `shouldSatisfy` isJust
                forM_ resultado \senha -> do
                    avaliarForcaSenha configPadrao senha `shouldBe` Right ()
                    Text.length senha `shouldSatisfy` (>= 8)
        it "devolve Nothing quando os bytes não são suficientes" do
            montarSenha 12 [1, 2, 3] `shouldBe` Nothing
            montarSenha 12 [] `shouldBe` Nothing
        it "descarta bytes que causariam viés (amostragem por rejeição)" do
            montarSenha 12 (replicate 200 255) `shouldBe` Nothing
        it "usa o tamanho mínimo de 12 caracteres para senhas geradas" do
            tamanhoSenhaGerada configPadrao `shouldBe` 12

    describe "cadastrarAluno (caso de uso com portas falsas em State)" do
        let executa existentes dados = runState (cadastrarAluno (portasFalsas existentes) dados) []
        it "cria o aluno, devolve a senha e persiste apenas o hash" do
            let (resultado, persistidos) = executa [] (NovoAluno "  Ana  " "ana@unifor.br")
            fmap (\(user, senha) -> (user.name, user.userRole, user.passwordHash, senha)) resultado
                `shouldBe` Right ("Ana", Aluno, "hash:Gerada123x", "Gerada123x")
            persistidos `shouldBe` [("ana@unifor.br", "hash:Gerada123x")]
        it "recusa e-mail já cadastrado sem persistir nada" do
            let (resultado, persistidos) = executa ["ana@unifor.br"] (NovoAluno "Ana" "ana@unifor.br")
            fmap (const ()) resultado `shouldBe` Left EmailJaCadastrado
            persistidos `shouldBe` []
        it "valida nome e e-mail antes de consultar as portas" do
            fmap (const ()) (fst (executa [] (NovoAluno " " "ana@unifor.br"))) `shouldBe` Left NomeVazio
            fmap (const ()) (fst (executa [] (NovoAluno "Ana" "ana.unifor.br"))) `shouldBe` Left EmailInvalidoAluno
        it "tem mensagens em português para cada erro" do
            map mensagemErroCadastroAluno [NomeVazio, EmailInvalidoAluno, EmailJaCadastrado]
                `shouldBe` ["Informe o nome do estudante", "Informe um e-mail válido", "Este e-mail já está cadastrado"]

portasFalsas :: [Text] -> PortasCadastroAluno (State [(Text, Text)])
portasFalsas existentes = PortasCadastroAluno
    { emailJaExiste = \email -> pure (email `elem` existentes)
    , gerarSenha    = pure "Gerada123x"
    , gerarHash     = \senha -> pure ("hash:" <> senha)
    , persistir     = \novo hash -> do
        State.modify ((novo.email, hash) :)
        pure (aluno "00000000-0000-0000-0000-0000000000a9" novo.nome |> set #email novo.email |> set #passwordHash hash)
    }
