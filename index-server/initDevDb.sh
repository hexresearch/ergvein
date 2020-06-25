export PGDATA=./db
export PGPORT=5434
export LANG=C
export LC_ALL=C

if [ ! -d "$PGDATA" ]; then
  initdb
  pg_ctl -D $PGDATA -l $PGDATA.log start
  sleep 1
  createuser localDb --superuser
  createdb localDb -O localDb -w
  sleep 2
else
  pg_ctl -D $PGDATA -l $PGDATA.log start
  sleep 1
fi
