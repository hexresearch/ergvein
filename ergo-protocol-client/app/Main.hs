{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Client
import Data.Maybe
import Data.Time
import Options.Generic

import qualified Data.Vector as V

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
        let --requiredBlock = "8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f"
            requiredBlock = "b21a1c00412b84033185f3cf6cdd345c4276628f3dda1e63b8502a4923c8e2bc"
            -- ty = ModifierBlockHeader
            ty = ModifierBlockTxs
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ty [requiredBlock]
        -- atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgSyncInfo $ SyncInfo [nullModifierId]
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
        MsgModifier m -> do
          putStrLn "MsgModifier"
          print m
      _ -> print ev
