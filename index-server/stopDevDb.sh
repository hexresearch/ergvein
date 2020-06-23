export PGDATA=./db
export PGPORT=5434
export LANG=C
export LC_ALL=C

/usr/lib/postgresql/12/bin/pg_ctl -D $PGDATA -l $PGDATA.log stop
