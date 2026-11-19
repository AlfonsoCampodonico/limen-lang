#!/bin/sh
# The postgres and redis modules against real servers: Postgres 17 (with
# SCRAM-SHA-256, MD5 and cleartext logins) and Redis 7, in Docker.
#   sh test/integration/databases.sh
# The containers listen on 127.0.0.1 only and are removed at the end.
# PGPORT and REDISPORT choose the ports (55433 and 56380 by default).
set -e
cd "$(dirname "$0")/../.."
dune build ./bin/main.exe
limen=$PWD/_build/default/bin/main.exe
pg=${PGPORT:-55433}
rd=${REDISPORT:-56380}
out=$(mktemp -d)
cleanup() {
  docker rm -f limen-it-pg limen-it-redis >/dev/null 2>&1 || true
  rm -rf "$out"
}
trap cleanup EXIT
docker rm -f limen-it-pg limen-it-redis >/dev/null 2>&1 || true
docker run -d --name limen-it-pg -e POSTGRES_USER=app -e POSTGRES_PASSWORD=secret -e POSTGRES_DB=app -p "127.0.0.1:$pg:5432" postgres:17 >/dev/null
docker run -d --name limen-it-redis -p "127.0.0.1:$rd:6379" redis:7 >/dev/null
i=0
until docker exec limen-it-pg pg_isready -U app -h 127.0.0.1 >/dev/null 2>&1; do
  i=$((i + 1))
  [ $i -lt 60 ] || { echo "postgres did not start"; exit 1; }
  sleep 1
done
# an MD5 user and a cleartext one, besides app's SCRAM-SHA-256
docker exec limen-it-pg sh -c "sed -i '1i host all mduser all md5\nhost all plain all password' /var/lib/postgresql/data/pg_hba.conf"
docker exec limen-it-pg psql -U app -q -c "SET password_encryption='md5'; CREATE USER mduser PASSWORD 'mdpw'; CREATE USER plain PASSWORD 'plainpw'; GRANT ALL ON SCHEMA public TO mduser, plain;" -c "SELECT pg_reload_conf();" >/dev/null

failed=0
run() {
  name=$1
  sed -e "s/PGPORT/$pg/g" -e "s/REDISPORT/$rd/g" "test/integration/$name.lmn" > "$out/$name.lmn"
  (cd "$out" && "$limen" build "$name.lmn" -o "$name" && "./$name" --grant net=127.0.0.1) > "$out/$name.got" 2>&1 || true
  if diff -u "test/integration/$name.expected" "$out/$name.got"; then
    echo "ok    $name"
  else
    echo "FAIL  $name"
    failed=1
  fi
}
run postgres
run redis
exit $failed
