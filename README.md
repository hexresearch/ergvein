# Ergvein index server

To run dev instance:
* server expects bitcoin node running (host, port, user and password can be changed for indexer in configuration.yaml)
  >  Example of botcoin.conf
  >  ```
  >  testnet=1
  >  server=1
  >  rpcuser=bitcoinrpc
  >  rpcpassword=password
  >  rpcallowip=127.0.0.1
  >  rpcport=8332
  >  #rpcconnect=127.0.0.1
  >  ```
  >  Note that actual rpcport will be "1[rpcport]" if testnet chosen
* run shells.sh at the ./ergvein to enter Nix shell
* navigate to ergvein/index-server folder
* run initDevDb.sh to create test Postgresql database
* run runDev.sh to start server
