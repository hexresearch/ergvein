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
            -- requiredBlock = "b21a1c00412b84033185f3cf6cdd345c4276628f3dda1e63b8502a4923c8e2bc"
            requiredBlock = "30075a8396918d62ba187d7e32cf3623e393f0ac1881e221bc5fb9515de633d0"
            -- ty = ModifierBlockHeader
            -- ty = ModifierBlockTxs
            ty = UnknownModifier 1
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
        MsgModifier (ModifierMsg ty m) -> do
          putStrLn "MsgModifier"
          print ty
          let bs = modifierBody $ m ! 0
          print $ bs
          print $ decode @BlockHeader bs
      _ -> print ev

{- HEADER
q = BlockHeader
  { version          = 1
  , parentId         = cee668bdcb8d24cc25569e82d7500b2c56eefddfa4629834de1cf6c96b2bfc80
  , adProofsRoot     = 47bfb6efa2ed041e51013e905f045ebf5af19e1a8510a98a516d355d31116908
  , transactionsRoot = a191856a3fd7703a92b8f8c397b8a0100195e98f408654e419ed01d03fc5f959
  , stateRoot        = d2ab2775e281381451e114c2c6fd84c6aa2acc9f349575387dbcfb79a8e57ffc13
  , timestamp        = 2021-02-02 14:20:55.509 UTC
  , extensionRoot    = 30075a8396918d62ba187d7e32cf3623e393f0ac1881e221bc5fb9515de633d0
  , nBits            = Difficulty {unDifficulty = 3381663975342080}
  , height           = 417791
  , votes            = ParamVotes 4 3 0
  , powSolution = AutolykosSolution
    { minerPubKey = 030e9662f3ed3448512424a273b3820ef83d3fee593cda88a115f344dc76ec4322
    , oneTimePubKey = 038036568285bbb106e3c3fe5a9a333b3663a02c56021f1eeb7222a85808365578
    , nonce = "\NUL\NUL\SO;\SOH\140\SO\201"
    , distance = 2642678796460668869164530876036253368923823090434204406855695
    }
  }
-}
