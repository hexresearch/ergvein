export PGDATA=./db
export PGPORT=5434
export LANG=C
export LC_ALL=C

if [ ! -d "$PGDATA" ]; then
  /usr/lib/postgresql/12/bin/initdb
  /usr/lib/postgresql/12/bin/pg_ctl -D $PGDATA -l $PGDATA.log start
  sleep 1
  /usr/lib/postgresql/12/bin/createuser localDb --superuser
  /usr/lib/postgresql/12/bin/createdb localDb -O localDb -w
  sleep 2
else
  /usr/lib/postgresql/12/bin/pg_ctl -D $PGDATA -l $PGDATA.log start
  sleep 1
fi