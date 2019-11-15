# Ergvein index server

To run dev instance:
* server expects bitcoin node running and recieving requests at port 18332 (testnet, can be changed at configuration.yaml) 
* run shells.sh at the ./ergvein to enter Nix shell
* navigate to ergvein/index-server folder
* run initDevDb.sh to create test Postgresql database
* run runDev.sh to start server
