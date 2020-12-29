module Ergvein.Wallet.Monad.Client (
    MonadIndexClient(..)
  , IndexerConnection(..)
  , IndexerMsg(..)
  , IndexReqSelector
  , broadcastIndexerMessage
  , requestSpecificIndexer
  , indexerPingerWidget
  , indexerConnPingerWidget
  , indexersAverageLatencyWidget
  , indexersAverageLatNumWidget
  , requestIndexerWhenOpen
  , addDiscovered
  , addManyDiscovered
  , addManual
  , setAddrActive
  , deleteAddr
  , setDiscovery
  -- * Reexports
  , SockAddr
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Function (on)
import Data.Functor.Misc (Const2(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time(NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef
import Control.Lens

import Ergvein.Index.Protocol.Types (Message(..))
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data IndexerConnection t = IndexerConnection {
  indexConAddr      :: !SockAddr
, indexConName      :: !Text
, indexConClosedE   :: !(Event t ())
, indexConOpensE    :: !(Event t ())
, indexConIsUp      :: !(Dynamic t Bool)
, indexConRespE     :: !(Event t Message)
, indexerConHeight  :: !(Dynamic t (Map Currency BlockHeight))
}

data IndexerMsg = IndexerClose | IndexerRestart | IndexerMsg Message

type IndexReqSelector t = EventSelector t (Const2 Text IndexerMsg)

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

class MonadBaseConstr t m => MonadIndexClient t m | m -> t where
  -- | Internal method to get reference to indexers
  getActiveConnsRef :: m (ExternalRef t (Map Text (IndexerConnection t)))
  -- | Get deactivated urls' reference. Internal
  getActiveUrlsNumRef :: m (ExternalRef t Int)
  -- | Get num reference. Internal
  getRequiredUrlNumRef :: m (ExternalRef t (Int, Int))
  -- | Get request timeout ref
  getRequestTimeoutRef :: m (ExternalRef t NominalDiffTime)
  -- | Get indexer request event
  getIndexReqSelector :: m (IndexReqSelector t)
  -- | Get indexer request trigger
  getIndexReqFire :: m (Map Text IndexerMsg -> IO ())
  -- | Get activation event and trigger
  getActivationEF :: m (Event t [Text], [Text] -> IO ())

discoveredPeerInfo , manualPeerInfo :: PeerInfo
discoveredPeerInfo = PeerInfo
      { _peerInfoIsActive = True
      , _peerInfoIsPinned = False
      }

manualPeerInfo = PeerInfo
    { _peerInfoIsActive = True
    , _peerInfoIsPinned = True
    }

addDiscovered :: (MonadIndexClient t m, MonadHasSettings t m) => Event t ErgveinNodeAddr -> m (Event t ())
addDiscovered addressE = do
    (_, activationFunc) <- getActivationEF
    updateSettingsAsync $ ffor addressE $ \ url ->
      settingsAddrs . at url .~ Just discoveredPeerInfo
    performEvent $ ffor addressE $ liftIO . activationFunc . pure

addManyDiscovered :: (MonadIndexClient t m, MonadHasSettings t m) => Event t [ErgveinNodeAddr] -> m (Event t ())
addManyDiscovered addressE = do
  (_, activationFunc) <- getActivationEF
  updateSettingsAsync $  ffor addressE $ \urls -> let
    in settingsAddrs %~ (`M.union` (M.fromList $ (,discoveredPeerInfo) <$> urls))
  performEvent $ ffor addressE $ liftIO . activationFunc

addManual :: (MonadIndexClient t m, MonadHasSettings t m) => Event t ErgveinNodeAddr -> m (Event t ())
addManual addressE = do
  (_, activationFunc) <- getActivationEF
  updateSettingsAsync $ ffor addressE $ \ url ->
    settingsAddrs . at url .~ Just manualPeerInfo
  performEvent $ ffor addressE $ liftIO . activationFunc . pure

setAddrActive :: (MonadIndexClient t m, MonadHasSettings t m) => Event t (ErgveinNodeAddr, Bool) -> m (Event t ())
setAddrActive addrE = updateSettingsAsync $ ffor addrE $ \(url, v) -> 
  settingsAddrs . at url . _Just %~ (peerInfoIsActive .~ v) . (peerInfoIsPinned .~ True)

deleteAddr :: (MonadIndexClient t m, MonadHasSettings t m) => Event t Text -> m (Event t ())
deleteAddr addrE = do
  closedE <- closeAndWait addrE
  updateSettingsAsync $ ffor (traceEvent "__________________deleteAddr" closedE) $ \url -> 
    settingsAddrs . at url .~ Nothing

-- | It is really important to wait until indexer performs deinitialization before deleting it from dynamic collections
closeAndWait :: MonadIndexClient t m => Event t ErgveinNodeAddr -> m (Event t ErgveinNodeAddr)
closeAndWait urlE = do
  req      <- getIndexReqFire
  connsRef <- getActiveConnsRef
  closedEE <- performEvent $ ffor urlE $ \url -> do
    liftIO $ req $ M.singleton url IndexerClose
    mconn <- fmap (M.lookup url) $ readExternalRef connsRef
    pure $ case mconn of
      Nothing -> never
      Just conn -> url <$ indexConClosedE conn
  switchDyn <$> holdDyn never closedEE

setDiscovery :: (MonadIndexClient t m, MonadHasSettings t m) =>  Event t Bool -> m (Event t ())
setDiscovery discE = updateSettingsAsync $ ffor discE $ \v -> 
  settingsDiscoveryEnabled .~ v

broadcastIndexerMessage :: (MonadIndexClient t m) => Event t IndexerMsg -> m ()
broadcastIndexerMessage reqE = do
  connsRef <- getActiveConnsRef
  fire <- getIndexReqFire
  performEvent_ $ ffor reqE $ \req -> do
    cm <- readExternalRef connsRef
    liftIO $ fire $ req <$ cm

requestIndexerWhenOpen :: MonadIndexClient t m => IndexerConnection t -> Message -> m (Event t ())
requestIndexerWhenOpen IndexerConnection{..} msg = do
  fire  <- getIndexReqFire
  initE <- fmap (gate (current indexConIsUp)) $ getPostBuild
  let reqE = leftmost [initE, indexConOpensE]
  performEvent $ ffor reqE $ const $
    liftIO $ fire $ M.singleton indexConName $ IndexerMsg msg

requestSpecificIndexer :: MonadIndexClient t m => Event t (ErgveinNodeAddr, Message) -> m (Event t Message)
requestSpecificIndexer saMsgE = do
  connsRef <- getActiveConnsRef
  fireReq  <- getIndexReqFire
  mrespE <- performFork $ ffor saMsgE $ \(sa, req) -> do
    mcon <- M.lookup sa <$> readExternalRef connsRef
    case mcon of
      Nothing -> pure Nothing
      Just con -> do
        liftIO $ fireReq $ M.singleton sa $ IndexerMsg req
        pure $ Just $ indexConRespE con
  switchHold never $ fmapMaybe id mrespE

indexerPingerWidget :: MonadIndexClient t m
  => Text                       -- Which indexer to ping
  -> Event t ()                     -- Manual refresh event
  -> m (Dynamic t NominalDiffTime)  -- Dynamic with the latency. Starting value 0
indexerPingerWidget addr refrE = do
  connmD  <- holdUniqDynBy eq =<< pure . fmap (M.lookup addr) =<< externalRefDynamic =<< getActiveConnsRef
  fmap join $ widgetHoldDyn $ ffor connmD $ \case
    Nothing -> pure $ pure 0
    Just conn -> indexerConnPingerWidget conn refrE
  where
    eq :: Maybe (IndexerConnection t) -> Maybe (IndexerConnection t) -> Bool
    eq = (==) `on` (fmap indexConAddr)

indexerConnPingerWidget :: MonadIndexClient t m
  => IndexerConnection t -> Event t () -> m (Dynamic t NominalDiffTime)
indexerConnPingerWidget IndexerConnection{..} refrE = do
  buildE <- getPostBuild
  fireReq <- getIndexReqFire
  te     <- fmap void $ tickLossyFromPostBuildTime 10
  let tickE = leftmost [te, buildE, refrE]
  pingE <- performFork $ ffor tickE $ const $ liftIO $ do
    p <- randomIO
    t <- getCurrentTime
    fireReq $ M.singleton indexConName $ (IndexerMsg $ MPing p)
    pure (p,t)
  pingD <- holdDyn Nothing $ Just <$> pingE
  pongE <- performFork $ ffor indexConRespE $ \case
    MPong p -> do
      t <- liftIO $ getCurrentTime
      mpt <- sampleDyn pingD
      pure $ join $ ffor mpt $ \(p',t') -> if (p == p') then Just (diffUTCTime t t') else Nothing
    _ -> pure Nothing
  holdDyn 0 $ fmapMaybe id pongE

indexersAverageLatencyWidget :: forall t m . MonadIndexClient t m => Event t () -> m (Dynamic t NominalDiffTime)
indexersAverageLatencyWidget = (fmap . fmap) snd . indexersAverageLatNumWidget

indexersAverageLatNumWidget :: forall t m . MonadIndexClient t m => Event t () -> m (Dynamic t (Int, NominalDiffTime))
indexersAverageLatNumWidget refrE = do
  connsD  <- externalRefDynamic =<< getActiveConnsRef
  fireReq <- getIndexReqFire
  te      <- fmap void $ tickLossyFromPostBuildTime 10
  buildE <- getPostBuild
  let tickE = leftmost [te, buildE, refrE]
  pingE <- performFork $ ffor tickE $ const $ do
    conns <- sampleDyn connsD
    p <- liftIO $ randomIO
    t <- liftIO $ getCurrentTime
    liftIO $ fireReq $ (IndexerMsg $ MPing p) <$ conns
    pure (p,t)
  pingD <- holdDyn Nothing $ Just <$> pingE
  pongsD <- list connsD $ \connD -> do
    let respE = switchDyn $ indexConRespE <$> connD
    pongE <- performFork $ ffor respE $ \case
      MPong p -> do
        t <- liftIO $ getCurrentTime
        mpt <- sampleDyn pingD
        pure $ join $ ffor mpt $ \(p',t') -> if (p == p') then Just (diffUTCTime t t') else Nothing
      _ -> pure Nothing
    holdDyn 0 $ fmapMaybe id pongE
  pure $ ffor (joinDynThroughMap pongsD) $ \pongmap -> let
    len = M.size pongmap
    pongs = sum $ M.elems pongmap
    avg = if len == 0 then 0 else pongs / (fromIntegral $ len)
    in (len, avg)
