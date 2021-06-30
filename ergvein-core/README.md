# ergvein-core

Part of [ergvein](cypra.io) wallet that doesn't require GUI parts.

Here goes:
* Management of connections to nodes and indexers
* Storage management (encryption, loading, unlocking)
* Keys management
* Scan for new filters
* Scan for restoration process
* Settings
* Transaction creation
* Height catchup 

# test suite

 To run test against Ergo node (including handshake and retrieveing sample of block head) use command:
 
    cabal new-run ergvein-core-node-test --flag client-tool -- --nodeAddress 127.0.0.1:9030
 
 Where nodeAddress is node to test and client-tool flag needed for correct compilation of rustlang lib links.
