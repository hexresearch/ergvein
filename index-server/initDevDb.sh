export PGDATA=./db
export PGPORT=5434
export LANG=C
export LC_ALL=C

if [ ! -d "$PGDATA" ]; then
  /nix/store/a2mdiy137jr4nw1hqig00c38byhs8cq3-postgresql-9.6.12/bin/initdb
  /nix/store/a2mdiy137jr4nw1hqig00c38byhs8cq3-postgresql-9.6.12/bin/pg_ctl -D $PGDATA -l $PGDATA.log start
  sleep 1
  /nix/store/a2mdiy137jr4nw1hqig00c38byhs8cq3-postgresql-9.6.12/bin/createuser localDb --superuser
  /nix/store/a2mdiy137jr4nw1hqig00c38byhs8cq3-postgresql-9.6.12/bin/createdb localDb -O localDb -w
  sleep 2
else
  /nix/store/a2mdiy137jr4nw1hqig00c38byhs8cq3-postgresql-9.6.12/bin/pg_ctl -D $PGDATA -l $PGDATA.log start
  sleep 1
fi