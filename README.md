# SGA Funcional - Sistema de Gestão Académica (IHP + Haskell)

Projeto desenvolvido para a disciplina de **Programação Funcional**, utilizando a arquitetura web pura em Haskell através do **IHP (Integrated Haskell Platform)**, **PostgreSQL** e **Nix** para reprodutibilidade de ambiente.

---

## Sobre o Projeto

O **SGA Funcional** é um sistema de gestão académica para controlo de alunos, cursos, matrículas e avaliação de desempenho académico. A lógica de negócio foca-se nos princípios do paradigma funcional puro, aplicando tipos algébricos de dados, imutabilidade, funções puras, currying, closures, pattern matching e list comprehensions.

---

## Tecnologias Utilizadas

* **Linguagem:** Haskell (GHC)
* **Framework Web:** IHP (Integrated Haskell Platform)
* **Base de Dados:** PostgreSQL
* **Gestão de Ambiente:** Nix / Devenv / Direnv
* **Gestão de Dependências:** Cabal

---

##  Arquitetura do Domínio Funcional

O núcleo de regras de negócio do sistema foi implementado no módulo funcional puro (`Application.Domain`):

* **Tipos Algébricos & Records:** Modelagem de entidades (`StudentRecord`, `StatusMatricula`, `SituacaoAcademica`) recorrendo a Sum Types e Product Types.
* **Currying e Closures:** Cálculo de médias ponderadas recorrendo à aplicação parcial de pesos fixos.
* **Pattern Matching & Monad Maybe:** Avaliação declarativa da situação do aluno tratando notas opcionais de exame final (`Maybe Double`).
* **List Comprehensions:** Filtragem e transformação declarativa de coleções de dados académicos.

---

## Configuração e Instalação

### Pré-requisitos
* [Nix](https://nixos.org/) com suporte a *Flakes* ativado.
* [Direnv](https://direnv.net/) (recomendado para carregamento automático do ambiente).

## Equipe

* João Pedro de Almeida Floriano - 2420377
* Elias Sousa Campos - 2619080
* Felipe Cavalcante - 2210374
* Nicolly Feitosa Barroso - 2420363
* Carlos Huan Celestino de Brito - 2320478