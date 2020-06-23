set -xe
sudo -u postgres createuser ergvein-indexer --superuser
sudo -u postgres createdb ergvein-indexer -O ergvein-indexer -w
