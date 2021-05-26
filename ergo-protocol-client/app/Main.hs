{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Client
import Data.Ergo.FFI
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
  testFunc
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
        let requiredBlock = "81a93bb7eb27bfb84b7afc6b64c75ee54023bb21224125214af218ddc41d60ec"
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ModifierBlockHeader [requiredBlock]
        -- atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgSyncInfo $ SyncInfo [nullModifierId]
      SockOutInbound (MsgOther (MsgInv (InvMsg itype is))) -> do
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg itype $ V.singleton $ V.head is
        pure ()
      _ -> pure ()
    print ev
