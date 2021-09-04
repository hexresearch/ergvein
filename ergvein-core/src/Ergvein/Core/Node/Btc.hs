{-
  Implementation of Btc connector
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Node.Btc
  (
    BtcType(..)
  , NodeBtc
  , initBtcNode
  , commandToText
  ) where

import Control.Monad.Catch (throwM, MonadThrow, Exception(..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Serialize (decode, runGet, runPut)
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX
import Data.Word
import Ergvein.Core.Node.Btc.Headers
import Ergvein.Core.Node.Socket
import Ergvein.Core.Node.Types
import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Ergvein.Types.Currency
import Network.Haskoin.Constants
import Network.Haskoin.Block
import Network.Haskoin.Network
import Network.Socket (getNameInfo, NameInfoFlag(..), SockAddr(..))
import Reflex
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Fork
import Reflex.Network
import Sepulcas.Native

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

-- These two are for dummy stats
import Control.Monad.Random
import Ergvein.Text

instance CurrencyRep BtcType where
  curRep _ = BTC

instance HasNode BtcType where
  type NodeReq BtcType = Message
  type NodeResp BtcType = Message
  type NodeSpecific BtcType = ()

initBtcNode :: (MonadSettings t m) => Bool -> SockAddr -> Event t NodeMessage -> m (NodeBtc t)
initBtcNode doLog sa msgE = do

  let net = btcNetwork
      nodeLog :: MonadIO m => Text -> m ()
      nodeLog = if doLog then logWrite . (nodeString BTC sa <>) else const (pure ())

  let restartE = fforMaybe msgE $ \case
        NodeMsgRestart -> Just ()
        _ -> Nothing
      closeE = fforMaybe msgE $ \case
        NodeMsgClose -> Just ()
        _ -> Nothing
      reqE = fforMaybe msgE $ \case
        NodeMsgReq (NodeReqBtc req) -> Just req
        _ -> Nothing
  buildE                    <- getPostBuild
  let startE = leftmost [buildE, restartE]

  -- Resolve address
  peerE <- performFork $ ffor startE $ const $ do
    (Just sname, Just sport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True sa
    pure $ Peer sname sport

  proxyD <- getSocksConf
  rec
    -- Start the connection
    s <- fmap switchSocket $ networkHold (pure noSocket) $ ffor peerE $ \peer -> do
      socket SocketConf {
          _socketConfPeer   = peer
        , _socketConfSend   = fmap (runPut . putMessage net) $ leftmost [reqE, handshakeE, hsRespE]
        , _socketConfPeeker = peekMessage net sa
        , _socketConfClose  = closeE
        , _socketConfProxy  = proxyD
        }

    let respE = _socketInbound s
    -- Start the handshake
    handshakeE <- performEvent $ ffor (socketConnected s) $ const $ mkVers net sa
    -- Finalize the handshake by sending "verack" message as a response
    -- Also, respond to ping messages by corrseponding pongs
    hsRespE <- performEvent $ fforMaybe respE $ \case
      MVersion Version{..} -> Just $ do
        nodeLog $ "Received version at height: " <> showt startHeight
        pure MVerAck
      MPing (Ping v) -> Just $ pure $ MPong (Pong v)
      MInv invs
        | binvs@(_:_) <- filter isBlockInv . invList $ invs
        -> Just $ do
            nodeLog $ "Got notification about new blocks: " <> showt (invHash <$> binvs)
            pure $ MGetData $ GetData binvs
      _ -> Nothing
    -- End rec

  performEvent_ $ ffor (_socketRecvEr s) $ nodeLog . showt

  let startHeightE = fforMaybe respE $ \case
        MVersion Version{..} -> Just startHeight
        _ -> Nothing
      newBlockE = fforMaybe respE $ \case
        MBlock Block{..} -> Just blockHeader
        MMerkleBlock MerkleBlock{..} -> Just merkleHeader
        _ -> Nothing
  treeRef <- newExternalRef Nothing
  performEvent_ $ ffor startHeightE $ writeExternalRef treeRef . Just . newHeadersTree
  performEvent_ $ ffor newBlockE $ \h -> do
    nodeLog $ "Adding new block header to store: " <> showt h
    mtree <- readExternalRef treeRef
    case mtree of
      Nothing -> nodeLog "Impossible: not handshaked while getting new block!"
      Just tree -> do
        t <- liftIO getCurrentTime
        case addHeader t h tree of
          Left er -> nodeLog $ "Invalid block: " <> pack er
          Right tree' -> writeExternalRef treeRef $ Just tree'
  treeD <- externalRefDynamic treeRef
  treeHeightD <- holdDyn Nothing $ fmap getBestHeight <$> updated treeD
  heightD <- holdJust $ leftmost [startHeightE, fmapMaybe id $ updated treeHeightD]

  -- Track handshake status
  let verAckE = fforMaybe respE $ \case
        MVerAck -> Just True
        _ -> Nothing

  shakeD <- holdDyn False $ leftmost [verAckE, False <$ closeE]
  let openE   = () <$ ffilter id (updated shakeD)
      closedE = () <$ _socketClosed s
  pure $ NodeConnection {
    nodeconCurrency   = BTC
  , nodeconUrl        = sa
  , nodeconOpensE     = openE
  , nodeconCloseE     = closedE
  , nodeconRespE      = respE
  , nodeconExtra      = ()
  , nodeconIsUp       = shakeD
  , nodeconDoLog      = doLog
  , nodeconHeight     = heightD
  }

-- | Internal peeker to parse messages coming from peer.
peekMessage :: (MonadPeeker m, MonadIO m, MonadThrow m, PlatformNatives)
    => Network
    -> SockAddr
    -> m Message
peekMessage net url = do
  x <- peek 24
  case decode x of
    Left e -> do
      nodeLog $ "Consumed " <> showt (B.length x)
      nodeLog $ "Could not decode incoming message header: " <> showt e
      nodeLog $ showt x
      throwM DecodeHeaderError
    Right (MessageHeader !_ !_ !len !_) -> do
      -- nodeLog $ showt cmd
      when (len > 32 * 2 ^ (20 :: Int)) $ do
        nodeLog "Payload too large"
        throwM (PayloadTooLarge len)
      let parseMessage bs = case runGet (getMessage net) bs of
            Left e -> do
              nodeLog $ "Cannot decode payload: " <> showt e
              throwM CannotDecodePayload
            Right !msg -> pure msg
      if len == 0 then parseMessage x else do
        y <- peek (fromIntegral len)
        parseMessage $ x `B.append` y
  where nodeLog = logWrite . (nodeString BTC url <>)

-- | Create version data message
mkVers :: MonadIO m => Network -> SockAddr -> m Message
mkVers net url = liftIO $ do
  ts   <- round <$> getPOSIXTime
  nonce <- randomIO
  pure $ MVersion $ Version
    { version = 70012
    , services = 0
    , timestamp = ts
    , addrRecv = NetworkAddress 0 (sockToHostAddress url)
    , addrSend = NetworkAddress 0 (sockToHostAddress $ SockAddrInet 0 0)
    , verNonce = nonce
    , userAgent = VarString (getHaskoinUserAgent net)
    , startHeight = 0
    , relay = True
    }

-- | Reasons why a peer may stop working.
data PeerException
    = PeerMisbehaving !String
      -- ^ peer is being a naughty boy
    | DuplicateVersion
      -- ^ peer sent an extra version message
    | DecodeHeaderError
      -- ^ incoming message headers could not be decoded
    | CannotDecodePayload
      -- ^ incoming message payload could not be decoded
    | PeerIsMyself
      -- ^ nonce for peer matches ours
    | PayloadTooLarge !Word32
      -- ^ message payload too large
    | PeerAddressInvalid
      -- ^ peer address not valid
    | PeerSentBadHeaders
      -- ^ peer sent wrong headers
    | NotNetworkPeer
      -- ^ peer cannot serve block chain data
    | PeerNoSegWit
      -- ^ peer has no segwit support
    | PeerTimeout
      -- ^ request to peer timed out
    | UnknownPeer
      -- ^ peer is unknown
    | PeerTooOld
      -- ^ peer has been connected too long
    | PeerSeppuku
      -- ^ peer has been asked to kill itself by it's master
    deriving (Eq, Show)
instance Exception PeerException

commandToText :: MessageCommand -> Text
commandToText = either (const "unknown") id . TE.decodeUtf8' . commandToString

isBlockInv :: InvVector -> Bool
isBlockInv = (\t -> InvBlock == t || InvWitnessBlock == t) . invType
