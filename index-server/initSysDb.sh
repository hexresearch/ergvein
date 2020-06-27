set -xe
sudo -u postgres createuser ergvein-indexer --superuser || true
sudo -u postgres createdb ergvein-indexer -O ergvein-indexer -w
