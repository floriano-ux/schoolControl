Link para o repositório : [link](https://github.com/floriano-ux/schoolControl/tree/feat/studentsCourses-crud)

# School Control - Sistema de Gestão Académica (IHP + Haskell)


# Sobre o Projeto

O **School Control** é um sistema de gestão académica para controlo de alunos, cursos, matrículas e avaliação de desempenho académico. A lógica de negócio foca-se nos princípios do paradigma funcional puro, aplicando tipos algébricos de dados, imutabilidade, funções puras, currying, closures, pattern matching e list comprehensions.

---

## Tecnologias Utilizadas

* **Linguagem:** Haskell (GHC)
* **Framework Web:** IHP (Integrated Haskell Platform)
* **Base de Dados:** PostgreSQL
* **Gestão de Ambiente:** Nix / Devenv / Direnv
* **Gestão de Dependências:** Cabal

---

## Arquitetura do Domínio Funcional

O núcleo de regras de negócio fica no módulo puro `Application.Domain` (sem IO). Ele trabalha sobre os
modelos que o IHP gera a partir de `Application/Schema.sql`: `Student`, `Course`, `Enrollment` e `Grade`.

### Regra de notas 

* `AVn = 70% PEn + 30% AVfn` (prova escrita + avaliação formativa)
* `(AV1 + AV2) / 2 < 4,0` → reprovado; caso contrário, aguarda a AV3
* `AV3 < 4,0` → reprovado
* `NF = ((AV1 + AV2) / 2 + AV3) / 2`; aprovado se `NF >= 5,0`

### Conceitos aplicados

* **Tipos algébricos & records:** `Pesos` (record), `Situacao`, `Conceito`, `ErroValidacao` (sum types) e
  `EnrollmentStatus` (gerado pelo IHP a partir de um ENUM do Postgres).
* **Currying e closures:** `mediaPonderada` devolve uma função que captura os pesos; `comporAV` e
  `mediaSimples` são aplicações parciais dela.
* **Pattern matching:** `situacaoDaMatricula` (enum + `Maybe`), `emailValido`.
* **Functor/Applicative em `Maybe`:** `notaAV3` só existe se PE3 e AVf3 foram lançadas.
* **IMC adaptado:** `classificarMedia` enquadra a média em faixas com guards.
* **Validações puras:** `validarNota` (`Either`), `validarStudent`, `validarCourse`, `validarGrade`.
* **List comprehensions:** `boletim` (JOIN entre alunos, matrículas e notas), `nomesAprovados`,
  `matriculasAtivasPorCurso`.
* **Recursão em cauda:** `mediaDaTurma`.

---

## Autenticação e Cadastro

O sistema tem login por e-mail/senha, bloqueio de conta após tentativas erradas (IHP `Lockable`),
papéis com permissões (`admin`, `coordenador`, `professor`, `aluno`) e **cadastro público** de novos
usuários (`/Register`), que entram com o papel `aluno`.

A criptografia e a configuração ficam isoladas no módulo `Application.Security`:

* **Hash de senhas com bcrypt:** `gerarHashSenha` (o efeito/aleatoriedade fica contido no `IO`) e
  `conferirSenha` (pura). Usados no login (`Sessions`) e no cadastro (`Users`, `Registrations`).
* **Criptografia de campos sensíveis (AES-256-GCM):** o CPF (`users.document`) é guardado cifrado por
  `cifrarCampo`/`cifrarOpcional` e só é decifrado (`decifrarCampo`/`decifrarOpcional`) para exibição ao
  administrador. O modo GCM é autenticado: dados adulterados são rejeitados (`Nothing`).

### Conceitos funcionais aplicados na autenticação

* **Monad `Reader` (configuração):** `AuthConfig` é lida do ambiente uma única vez no limite de `IO`
  (`carregarAuthConfig`) e consumida por funções puras via `Reader` (`validarForcaSenha`,
  `tamanhoMinimoSenha`).
* **`IO` isolado:** só `carregarAuthConfig`, `gerarHashSenha` e `cifrarCampo` vivem em `IO`; o resto é puro.
* **`Maybe`/`Either` para validação:** `decifrarCampo` devolve `Maybe`; `validarForcaSenha` devolve
  `Either ErroSenha ()`; `validarCadastro` devolve `Either [ErroCadastro]` acumulando **todos** os erros.
* **Functor em coleções:** `validarCadastro` percorre a coleção de regras com `filter`/`map`.
* **Continuation Passing Style (CPS):** encadeia etapas de validação em fluxos de negócio através de continuações explícitas de erro (`onErro`) e sucesso (`onSucesso`), sem intermediários:
  * `pipelineCadastroCPS`: valida campos do cadastro, força da senha e unicidade de e-mail.
  * `pipelineMatriculaCPS`: valida sequencialmente a existência do aluno, do curso e a ausência de duplicidade no `EnrollmentsController`.
* **Lazy evaluation:** `variacoesDeEmail` é uma lista **infinita** de alternativas de e-mail; graças à
  avaliação preguiçosa, `primeiroEmailLivre` consome apenas até achar a primeira variação disponível
  (usada para sugerir um e-mail quando o informado já existe).

### Usuários de teste (fixtures)

Senha de todos: `senha123` (apenas para desenvolvimento). E-mails:
`admin@`, `coordenador@`, `professor@`, `aluno@` `schoolcontrol.dev`.

### Configuração por ambiente

* `AUTH_BCRYPT_COST` — custo do bcrypt (padrão `10`).
* `AUTH_ENCRYPTION_KEY` — chave da criptografia AES (ajustada para 32 bytes). **Troque em produção.**

---

## Dashboard e Reatividade em Tempo Real (IHP AutoRefresh)

O sistema possui um dashboard analítico (`/Reports`) com KPIs acadêmicos (total de alunos, matrículas ativas, carga horária total e percentual de aprovação) e agregações calculadas puramente em Haskell no módulo `Application.Domain`.

### Reatividade sem Miso (IHP AutoRefresh)

Para proporcionar reatividade em tempo real sem a necessidade de reescrever o frontend em Miso/Obelisk ou introduzir frameworks pesados de JavaScript:

* **`autoRefresh` no Controller:** Em `Web/Controller/Reports.hs`, a action `ReportsAction` invoca `autoRefresh`. O framework IHP rastreia automaticamente as tabelas consultadas (`Student`, `Course`, `Enrollment`, `Grade`) via PostgreSQL `LISTEN/NOTIFY`.
* **WebSocket e Diff de DOM:** Quando ocorrem mutações no banco de dados (por exemplo, lançamento de notas, criação ou trancamento de matrículas), o servidor IHP reexecuta a action, recomputa as métricas no domínio funcional e despacha o diff HTML via WebSocket.
* **Atualização transparente no navegador:** O `ihp-auto-refresh.js` integrado com `morphdom` aplica a atualização cirurgicamente nos nós alterados do DOM sem recarregar a página e sem necessidade de código JavaScript manual.

## Equipe

* João Pedro de Almeida Floriano - 2420377
* Elias Sousa Campos - 2619080
* Felipe Cavalcante - 2210374
* Nicolly Feitosa Barroso - 2420363
* Carlos Huan Celestino de Brito - 2320478
