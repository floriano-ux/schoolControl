# Arquitetura do SchoolControl

Documento de arquitetura e registro de decisões (ADRs) — Sprint 5.

## 1. Visão geral

O SchoolControl é uma aplicação web **server-side rendered** escrita em Haskell com o framework
**IHP 1.6**, sobre **PostgreSQL**. O ambiente de desenvolvimento e o build são reproduzíveis via
**Nix + devenv + direnv** (no Windows, dentro do WSL2).

```
 Navegador ──HTTP──▶ Web.FrontController ──▶ Controllers (Web/Controller/*)
     ▲                                          │   │
     │ HTML (HSX)                               │   ├─▶ Application.Domain   (regras puras)
     │ + diffs via WebSocket (autoRefresh)      │   ├─▶ Application.Security (Reader, bcrypt, AES)
     │                                          │   └─▶ Application.Errors   (AppM = ExceptT AppError IO)
     └──────────── Views (Web/View/*, HSX) ◀────┘
                                                │
                                   IHP ModelSupport / QueryBuilder (hasql)
                                                │
                                           PostgreSQL
                       (Application/Schema.sql: tipos, FKs, UNIQUE, CHECK)
```

## 2. Camadas e responsabilidades

| Camada | Módulos | Pode fazer IO? | Responsabilidade |
|---|---|---|---|
| Domínio | `Application/Domain.hs` | **Não** | Notas, médias, situação, validações, relatórios, permissões, pipelines CPS, avaliação preguiçosa |
| Segurança | `Application/Security.hs` | Só nas bordas | Força de senha (`Reader AuthConfig`), hash bcrypt, cifra AES-256-GCM |
| Erros | `Application/Errors.hs` | Só em `capturarInfra` | Tipo único `AppError`, monad `AppM`, tradução de exceções SQL, mensagens e status HTTP |
| Controllers | `Web/Controller/*` | Sim | Ler parâmetros, buscar dados, chamar o domínio, decidir a resposta |
| Views | `Web/View/*` | Não | HTML com HSX (tipado e verificado em tempo de compilação) |
| Esquema | `Application/Schema.sql` | — | Fonte da verdade dos tipos gerados (`Generated.Types`) e das restrições de integridade |

Regra de dependência: **Views e Controllers dependem do Domínio; o Domínio não depende de nada web**.
Por isso todo o domínio é testável com `runghc Test/Main.hs`, sem banco e sem servidor.

## 3. Fluxo de uma requisição (exemplo: criar matrícula)

1. `POST /CreateEnrollment` → `EnrollmentsController.CreateEnrollmentAction`.
2. `exigirPermissao Gerenciar Matriculas` (papel do usuário × recurso, função pura `podeAcessar`).
3. Busca alunos, cursos e matrículas existentes (IO).
4. `pipelineMatriculaCPS` valida em **CPS**: aluno existe → curso existe → não é duplicada.
   Cada etapa recebe as continuações `onErro`/`onSucesso`.
5. Na continuação de sucesso, a gravação roda em **`AppM`**:
   `executarApp (capturarInfra "matricula" (createRecord m))`.
   Se outra requisição gravou a mesma matrícula entre a validação e o `INSERT`, o `UNIQUE`
   do banco dispara, `capturarInfra` registra o detalhe no log e devolve `Left (Conflito …)`.
6. O controller transforma `Either AppError a` em resposta: redireciona com mensagem de sucesso
   ou re-renderiza o formulário com a mensagem amigável no campo.

## 4. Tratamento de erros

Três níveis, do mais puro ao mais efetivo:

| Situação | Ferramenta | Exemplo |
|---|---|---|
| Valor pode faltar | `Maybe` | `decifrarCampo` (chave errada/adulteração → `Nothing`), `notaAV3` |
| Validação com motivo | `Either e a` | `validarNota`, `validarForcaSenha`, `validarCadastro` (acumula erros) |
| Fluxo com IO que pode falhar | `AppM = ExceptT AppError IO` | criar/excluir matrícula, cadastro de usuário |

`AppError` reúne todas as falhas conhecidas:

| Construtor | Origem típica | HTTP | Mensagem ao usuário |
|---|---|---|---|
| `ErroDeValidacao [Text]` | regra do domínio, `CHECK` (23514), FK (23503) | 422 | as mensagens de validação |
| `NaoEncontrado Text` | `exigirJust` sobre `fetchOneOrNothing` | 404 | "X não encontrado(a)." |
| `Conflito Text` | `UNIQUE` (23505) | 409 | "Já existe um registro com esses dados." |
| `NaoAutenticado` / `SemPermissao` | `exigirPermissao` | 401/403 | texto fixo |
| `FalhaDeInfra Text` | banco fora do ar, qualquer outra exceção | 503 | genérica; **o detalhe só vai para o log** |

Princípios:

* Exceções são capturadas **em um único ponto** (`capturarInfra`), nunca espalhadas pelo código.
* A classificação (`classificarFalhaSql`) e as mensagens (`mensagemAppError`) são **puras e testadas**.
* Detalhe técnico nunca chega ao usuário (há teste garantindo isso).

## 5. Segurança

* **Senhas:** bcrypt (custo configurável por `AUTH_BCRYPT_COST`, padrão 10) com salt por senha.
* **Dados sensíveis:** CPF (`users.document`) cifrado com AES-256-GCM, nonce aleatório de 12 bytes por
  valor e tag de autenticação de 16 bytes (detecta adulteração). Chave por `AUTH_ENCRYPTION_KEY`.
* **Sessão:** login do IHP (cookie de sessão cifrado e HttpOnly), bloqueio de conta após 10 tentativas
  (`Lockable`), logs de tentativas inválidas.
* **Autorização:** papéis (`admin`, `coordenador`, `professor`, `aluno`) × recursos, decidido pela função
  pura `podeAcessar` e aplicado em `beforeAction`/`exigirPermissao`.
* **Defesa em profundidade:** as regras também existem no banco (`UNIQUE`, `CHECK` de notas 0–10,
  `CHECK workload > 0`, FKs com `ON DELETE CASCADE`).

## 6. Testes

| Tipo | Arquivo | Como roda | Cobre |
|---|---|---|---|
| Unitários | `Test/Main.hs` | `nix flake check` (derivação `tests`) | domínio, relatórios, filtros, permissões, CPS, lazy, `AppError`/`AppM`, Reader, bcrypt, AES |
| Integração | `Test/Integration.hs` | `nix flake check` (derivação `integration-tests`, PostgreSQL temporário) | schema + fixtures, CRUD real, UNIQUE/FK/CHECK → `AppError`, cascata, relatórios sobre dados persistidos, senha/CPF persistidos |
| Stress | `scripts/stress.sh` | manual, com o app rodando | throughput e latência (p50–p99) de páginas públicas, listagem e dashboard |

O teste de integração cria um banco próprio (`schoolcontrol_integracao`) e nunca altera o banco de
desenvolvimento. Resultados do stress ficam em `docs/stress/`.

## 7. Registro de decisões (ADRs)

### ADR-01 — IHP como framework web
**Contexto:** o curso pede Haskell com SSR, banco relacional e entregas rápidas.
**Decisão:** IHP 1.6. **Motivos:** gera tipos Haskell a partir do `Schema.sql` (consultas tipadas), HSX
verificado em compilação, autenticação pronta, `autoRefresh`, ambiente reproduzível com Nix.
**Consequência:** o ambiente exige Nix (no Windows, WSL2) e o primeiro build é pesado.

### ADR-02 — Núcleo de domínio puro
**Decisão:** toda regra de negócio fica em `Application.Domain`, sem IO; controllers só orquestram.
**Consequência:** testes rápidos e determinísticos; o mesmo código serve o formulário, o dashboard e os testes.

### ADR-03 — `Either`/`Maybe` para validação e `ExceptT` (AppM) para fluxos com IO
**Alternativas:** exceções do Haskell em todo lugar; `Either` aninhado manualmente.
**Decisão:** validação pura com `Either`; fluxos com efeitos em `ExceptT AppError IO`, com captura de
exceções concentrada em `capturarInfra`.
**Consequência:** curto-circuito sem `case` aninhado, erros explícitos nos tipos, mensagens consistentes.

### ADR-04 — CPS no pipeline de matrícula e de cadastro
**Decisão:** encadear as validações com continuações explícitas (`onErro`, `onSucesso`).
**Consequência:** o fluxo lê de cima para baixo e o chamador escolhe o tipo do resultado
(ex.: `Left`/`Right` nos testes, `render`/`redirect` no controller).

### ADR-05 — `Reader` para configuração de segurança
**Decisão:** ler o ambiente uma vez (`carregarAuthConfig`) e passar `AuthConfig` via `Reader`.
**Consequência:** funções de validação puras e testáveis com qualquer configuração.

### ADR-06 — bcrypt + AES-256-GCM (pacote `crypton`)
**Decisão:** bcrypt para senhas (lento de propósito, com salt); AES-GCM para campos que precisam ser lidos
de volta. **Consequência:** a chave AES precisa ser gerenciada fora do código em produção.

### ADR-07 — Reatividade com `autoRefresh` em vez de Miso/Obelisk
**Decisão:** usar o `autoRefresh` do IHP (LISTEN/NOTIFY + WebSocket + morphdom).
**Consequência:** dashboard reativo sem um segundo frontend em Haskell/GHCJS; menos código e build mais simples.

### ADR-08 — Restrições duplicadas no banco
**Decisão:** além das validações do domínio, manter `UNIQUE`, `CHECK` e FKs no PostgreSQL.
**Consequência:** protege contra condições de corrida e escrita fora da aplicação; as violações viram
`AppError` via `classificarFalhaSql`.

## 8. Como rodar

```bash
# dentro do WSL, na pasta do projeto
direnv allow          # primeira vez
./start               # sobe app (http://localhost:8000) + IDE do IHP (http://localhost:8001)
nix flake check --impure -L   # testes unitários + integração
./scripts/stress.sh   # teste de stress (com o app rodando)
```
