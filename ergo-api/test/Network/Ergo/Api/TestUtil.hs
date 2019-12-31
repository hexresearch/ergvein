module Network.Ergo.Api.TestUtil where

import Network.Ergo.Api.Client

testClient :: IO Client 
testClient = newClient "127.0.0.1" 9052