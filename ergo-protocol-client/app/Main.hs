{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Client
import Data.Ergo.Block (BlockHeader)
import Data.Maybe
import Data.Persist
import Data.Time
import Options.Generic
import Data.Vector.Generic ((!))
import qualified Data.Vector as V
import Text.Groom

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteArray  as BA
import Crypto.Hash

data Options = Options {
  nodeAddress  :: Maybe String <?> "Address of node"
, nodePort     :: Maybe Int <?> "Port of node"
, testnet      :: Bool <?> "Is this testnet network"
} deriving (Generic)

instance ParseRecord Options

getNodeAddress :: Options -> String
getNodeAddress Options{..} = fromMaybe "127.0.0.1" $ unHelpful nodeAddress

getNodePort :: Options -> Int
getNodePort Options{..} = fromMaybe (if unHelpful testnet then 19030 else 9030) $ unHelpful nodePort

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergo protocol client example"
  let net = if unHelpful testnet then Testnet else Mainnet
  inChan <- newTChanIO
  let conf = SocketConf {
          _socketConfPeer = Peer (getNodeAddress opts) (show $ getNodePort opts)
        , _socketConfSocks = Nothing
        , _socketConfReopen = Just (3.0, 5)
        }
  outChan <- ergoSocket net inChan conf
  forever $ do
    ev <- atomically $ readTChan outChan
    case ev of
      SockOutInbound (MsgHandshake h) -> do
        t <- getCurrentTime
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgHandshake $ makeHandshake 0 t
        threadDelay 1000000
        let requiredBlock = "8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f" -- H=414474
            -- requiredBlock = "2cc7c4f1f609694b83df093192e5bf3f4ad441a3c8f1959a28d51eb16fa94b19"
            --
            merkleR c = ModifierId
                      $ BA.convert
                      $ hash @_ @Blake2b_256
                      $ BS.singleton c
                     <> unModifierId requiredBlock
                     <> unModifierId "722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951"
            ty = ModifierBlockTxs
        print $ merkleR 102
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ty
          -- [requiredBlock]
          $ V.fromList [merkleR c | c <- [102]]
        -- atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgSyncInfo $ SyncInfo [
          -- "efa4abde000dca13fd08220d48f70c3e64b49d91102af9d047fd6f35826352e9"
          -- ]
        pure ()
      SockOutInbound (MsgOther (MsgInv (InvMsg itype is))) -> do
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg itype $ V.singleton $ V.head is
        pure ()
      _ -> pure ()
    case ev of
      SockOutInbound (MsgHandshake h) -> putStrLn "HANDSHAKE" >> print h
      SockOutInbound (MsgOther msg) -> case msg of
        MsgInv      _ -> putStrLn "MsgInv"
        MsgSyncInfo (SyncInfo bids) -> do
          putStrLn "MsgSyncInfo"
          -- mapM_ print bids
        MsgRequestModifier _ -> putStrLn "MsgRequestModifier"
        MsgModifier (ModifierMsg ty m) -> do
          putStrLn "MsgModifier"
          print ty
          let bs = modifierBody $ m ! 0
          print $ ModifierId $ BS.drop 32 $ bs
          print $ decode @BlockHeader bs
      _ -> print ev

{- HEADER
BlockHeader
  { version          = 1
  , parentId         = 8bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e
  , adProofsRoot     = 4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0
  , transactionsRoot = 722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951
  , stateRoot        = 6c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512
  , timestamp        = 2021-01-28 22:49:59.636 UTC
  , extensionRoot    = a1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4
  , nBits            = Difficulty {unDifficulty = 2572389057560576}
  , height           = 414474
  , votes            = ParamVotes 4 3 0
  , powSolution = AutolykosSolution
    { minerPubKey = 02bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3
    , oneTimePubKey = 026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb14
    , nonce = "\NUL\b\161\209\ETX\136\SOH\ETB"
    , distance = 35863003992655055679291741607273543535646500642591973829915050
    }
  }
-}
