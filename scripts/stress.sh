#!/usr/bin/env bash
# Teste de stress do SchoolControl com wrk (Sprint 5).
#
# Pré-requisitos:
#   * app rodando (`./start` ou `devenv up`) em $BASE_URL (padrão http://localhost:8000)
#   * wrk e curl instalados  (Ubuntu/WSL: sudo apt install -y wrk curl)
#
# Uso:
#   ./scripts/stress.sh                 # 30s, 4 threads, 50 conexões
#   DURACAO=60s CONEXOES=200 ./scripts/stress.sh
#
# O que mede:
#   1. GET /                 página pública (SSR com HSX, sem banco)
#   2. GET /NewSession       formulário de login
#   3. GET /Students         listagem autenticada (consulta ao banco)
#   4. GET /Reports          dashboard autenticado: 4 consultas + agregações puras
#                            (média por curso, ranking, filtros, KPIs)
#   5. GET /Reports?status=ativa   dashboard com filtro aplicado
#
# Os resultados vão para docs/stress/<data>.txt, para anexar ao relatório.

set -euo pipefail

BASE_URL="${BASE_URL:-http://localhost:8000}"
DURACAO="${DURACAO:-30s}"
THREADS="${THREADS:-4}"
CONEXOES="${CONEXOES:-50}"
EMAIL="${EMAIL:-admin@schoolcontrol.dev}"
SENHA="${SENHA:-senha123}"   # senha das fixtures de desenvolvimento

for cmd in wrk curl; do
    command -v "$cmd" >/dev/null || { echo "Falta '$cmd'. Instale com: sudo apt install -y $cmd" >&2; exit 1; }
done

curl -fsS -o /dev/null "$BASE_URL/" || { echo "App não respondeu em $BASE_URL. Rode ./start antes." >&2; exit 1; }

# Login uma vez e reaproveita o cookie de sessão em todas as requisições autenticadas.
JAR="$(mktemp)"
trap 'rm -f "$JAR"' EXIT
curl -fsS -o /dev/null -c "$JAR" -b "$JAR" \
    --data-urlencode "email=$EMAIL" --data-urlencode "password=$SENHA" \
    "$BASE_URL/CreateSession"
# O cookie de sessão do IHP é HttpOnly: no cookie jar do curl ele aparece com o prefixo "#HttpOnly_".
COOKIE="$(awk '{ sub(/^#HttpOnly_/, "") } !/^#/ && NF >= 7 { printf "%s=%s; ", $6, $7 }' "$JAR")"
[ -n "$COOKIE" ] || { echo "Login falhou: nenhum cookie de sessão recebido." >&2; exit 1; }

# Confere que o cookie realmente autentica (sem login, /Reports redireciona).
STATUS="$(curl -s -o /dev/null -w '%{http_code}' -H "Cookie: $COOKIE" "$BASE_URL/Reports")"
[ "$STATUS" = "200" ] || { echo "Cookie não autenticou (/Reports devolveu $STATUS)." >&2; exit 1; }

mkdir -p docs/stress
SAIDA="docs/stress/$(date +%Y-%m-%d_%H%M%S).txt"
{
    echo "SchoolControl - teste de stress com wrk"
    echo "Data: $(date -Iseconds)   Base: $BASE_URL"
    echo "Parâmetros: duração=$DURACAO threads=$THREADS conexões=$CONEXOES"
    echo "Máquina: $(nproc) CPUs, $(free -h | awk '/Mem:/ {print $2}') RAM"
} | tee "$SAIDA"

rodar() {
    local nome="$1" caminho="$2" autenticado="${3:-nao}"
    local header=()
    [ "$autenticado" = "sim" ] && header=(-H "Cookie: $COOKIE")
    echo -e "\n===== $nome  ($caminho)" | tee -a "$SAIDA"
    wrk -t"$THREADS" -c"$CONEXOES" -d"$DURACAO" --latency "${header[@]}" "$BASE_URL$caminho" | tee -a "$SAIDA"
}

rodar "Página inicial"         "/"
rodar "Formulário de login"    "/NewSession"
rodar "Listagem de alunos"     "/Students"             sim
rodar "Dashboard"              "/Reports"              sim
rodar "Dashboard com filtro"   "/Reports?status=ativa" sim

echo -e "\nResultados salvos em $SAIDA"
echo "Resumo (Requests/sec e latência p99):"
grep -E "^=====|Requests/sec|99%" "$SAIDA"
