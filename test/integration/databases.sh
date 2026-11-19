#!/bin/sh
# The postgres and redis modules against real servers: Postgres 17 (with
# SCRAM-SHA-256, MD5 and cleartext logins) and Redis 7, in Docker, plain
# and over TLS (a throwaway CA, trusted through SSL_CERT_FILE).
#   sh test/integration/databases.sh
# The containers listen on 127.0.0.1 only and are removed at the end.
# PGPORT, REDISPORT, PGTLSPORT and REDISTLSPORT choose the ports (55433,
# 56380, 55434 and 56381 by default).
set -e
cd "$(dirname "$0")/../.."
dune build ./bin/main.exe
limen=$PWD/_build/default/bin/main.exe
pg=${PGPORT:-55433}
rd=${REDISPORT:-56380}
pgt=${PGTLSPORT:-55434}
rdt=${REDISTLSPORT:-56381}
out=$(mktemp -d)
all="limen-it-pg limen-it-redis limen-it-pg-tls limen-it-redis-tls"
cleanup() {
  docker rm -f $all >/dev/null 2>&1 || true
  rm -rf "$out"
}
trap cleanup EXIT
docker rm -f $all >/dev/null 2>&1 || true
# a CA and a certificate for localhost
(cd "$out" &&
  openssl req -x509 -newkey rsa:2048 -nodes -keyout ca.key -out ca.pem -days 1 -subj "/CN=limen test ca" 2>/dev/null &&
  openssl req -newkey rsa:2048 -nodes -keyout server.key -out server.csr -subj "/CN=localhost" 2>/dev/null &&
  printf "subjectAltName=DNS:localhost,IP:127.0.0.1\n" > ext.cnf &&
  openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out server.crt -days 1 -extfile ext.cnf 2>/dev/null &&
  chmod 644 server.key)
docker run -d --name limen-it-pg -e POSTGRES_USER=app -e POSTGRES_PASSWORD=secret -e POSTGRES_DB=app -p "127.0.0.1:$pg:5432" postgres:17 >/dev/null
docker run -d --name limen-it-redis -p "127.0.0.1:$rd:6379" redis:7 >/dev/null
docker run -d --name limen-it-pg-tls -e POSTGRES_USER=app -e POSTGRES_PASSWORD=secret -e POSTGRES_DB=app -v "$out:/certs:ro" -p "127.0.0.1:$pgt:5432" postgres:17 \
  sh -c 'cp /certs/server.crt /certs/server.key /tmp/ && chown postgres /tmp/server.* && chmod 600 /tmp/server.key && exec docker-entrypoint.sh postgres -c ssl=on -c ssl_cert_file=/tmp/server.crt -c ssl_key_file=/tmp/server.key' >/dev/null
docker run -d --name limen-it-redis-tls -v "$out:/certs:ro" -p "127.0.0.1:$rdt:6380" redis:7 \
  redis-server --port 0 --tls-port 6380 --tls-cert-file /certs/server.crt --tls-key-file /certs/server.key --tls-ca-cert-file /certs/ca.pem --tls-auth-clients no >/dev/null
for c in limen-it-pg limen-it-pg-tls; do
  i=0
  until docker exec $c pg_isready -U app -h 127.0.0.1 >/dev/null 2>&1; do
    i=$((i + 1))
    [ $i -lt 60 ] || { echo "$c did not start"; exit 1; }
    sleep 1
  done
done
sleep 2
# an MD5 user and a cleartext one, besides app's SCRAM-SHA-256
docker exec limen-it-pg sh -c "sed -i '1i host all mduser all md5\nhost all plain all password' /var/lib/postgresql/data/pg_hba.conf"
docker exec limen-it-pg psql -U app -q -c "SET password_encryption='md5'; CREATE USER mduser PASSWORD 'mdpw'; CREATE USER plain PASSWORD 'plainpw'; GRANT ALL ON SCHEMA public TO mduser, plain;" -c "SELECT pg_reload_conf();" >/dev/null

failed=0
run() {
  name=$1
  host=$2
  extra=$3
  # each in a directory of its own, so `use postgres` finds the standard
  # library, not a test named postgres.lmn next to it
  mkdir -p "$out/$name"
  sed -e "s/PGTLSPORT/$pgt/g" -e "s/REDISTLSPORT/$rdt/g" -e "s/PGPORT/$pg/g" -e "s/REDISPORT/$rd/g" "test/integration/$name.lmn" > "$out/$name/main.lmn"
  if [ -n "$extra" ]; then cp "$out/$extra" "$out/$name/"; fi
  (cd "$out/$name" && "$limen" build main.lmn -o test && SSL_CERT_FILE="$out/ca.pem" ./test --grant "net=$host" --grant fs=.) > "$out/$name.got" 2>&1 || true
  if diff -u "test/integration/$name.expected" "$out/$name.got"; then
    echo "ok    $name"
  else
    echo "FAIL  $name"
    failed=1
  fi
}
run postgres 127.0.0.1
run redis 127.0.0.1
run tls localhost
# 0.7's test: a gzipped log, read line by line, written to Postgres (over
# TLS); 300,000 lines in two gzip members, the same every time
python3 -I -c "
import random
random.seed(7)
with open('$out/big.log', 'w') as f:
    for i in range(300000):
        f.write('2026-11-19T12:00:00 %s request %d took %d ms\\n' % (random.choice(['INFO', 'INFO', 'WARN', 'ERROR']), i, random.randint(1, 900)))
"
head -c 8000000 "$out/big.log" | gzip -c > "$out/big.log.gz"
tail -c +8000001 "$out/big.log" | gzip -c >> "$out/big.log.gz"
run gzip_to_postgres localhost big.log.gz
exit $failed
