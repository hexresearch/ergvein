module Ergvein.Core.Client.Monad(
    ClientMessage(..)
  , MonadClientConstr
  , MonadClient(..)
  , IndexerConnection(..)
  , IndexerStatus(..)
  , IndexerMsg(..)
  , IndexReqSelector
  , getArchivedUrlsD
  , getInactiveUrlsD
  , activateURL
  , activateURLList
  , deactivateURL
  , forgetURL
  , broadcastIndexerMessage
  , requestSpecificIndexer
  , indexerPingerWidget
  , indexerConnPingerWidget
  , indexersAverageLatencyWidget
  , indexersAverageLatNumWidget
  , requestIndexerWhenOpen
  , indexerStatusUpdater
  , indexerLastStatus
  -- * Reexports
  , SockAddr
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Random
import Data.Function (on)
import Data.Functor.Misc (Const2(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time(NominalDiffTime, getCurrentTime, diffUTCTime)
import GHC.Generics
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef

import Ergvein.Core.Node.Socket
import Ergvein.Core.Settings
import Ergvein.Index.Protocol.Types (Message(..), ProtocolVersion)
import Ergvein.Node.Constants
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Reflex.Flunky
import Reflex.Fork

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data ClientMessage
  = CMSLoading Int Int Int
  | CMSError
  | CMSEmpty
  | CMSValidationError
  | CMSDone
  | CMSTimeout
  | CMSRestarting
  | CMSAllOutOfSync
  deriving (Eq)

data IndexerConnection t = IndexerConnection {
  indexConAddr :: !SockAddr
, indexConName :: !Text
, indexConIndexerVersion :: !(Dynamic t (Maybe ProtocolVersion))
, indexConClosedE :: !(Event t CloseReason)
, indexConOpensE :: !(Event t ())
, indexConIsUp :: !(Dynamic t Bool)
, indexConRespE :: !(Event t Message)
, indexConHeight :: !(Dynamic t (Map Currency BlockHeight))
, indexConStatus :: !(Dynamic t IndexerStatus)
}

data IndexerStatus = IndexerOk | IndexerNotSynced | IndexerWrongVersion !(Maybe ProtocolVersion) | IndexerMissingCurrencies
  deriving (Eq, Ord, Show, Read, Generic)

data IndexerMsg = IndexerClose | IndexerRestart | IndexerMsg Message

type IndexReqSelector t = EventSelector t (Const2 ErgveinNodeAddr IndexerMsg)

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

type MonadClientConstr t m = (
    Adjustable t m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , MonadIO m
  , MonadSample t (Performable m)
  , MonadUnliftIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  )

class MonadClientConstr t m => MonadClient t m | m -> t where
  -- | Get active addrs ref
  getActiveAddrsRef :: m (ExternalRef t (Set ErgveinNodeAddr))
  -- | Get passive urls' reference. Internal
  getArchivedAddrsRef :: m (ExternalRef t (Set ErgveinNodeAddr))
  -- | Internal method to get reference to indexers
  getActiveConnsRef :: m (ExternalRef t (Map ErgveinNodeAddr (IndexerConnection t)))
  -- | Internal method to get last status of indexer
  getStatusConnsRef :: m (ExternalRef t (Map ErgveinNodeAddr IndexerStatus))
  -- | Get deactivated urls' reference. Internal
  getInactiveAddrsRef :: m (ExternalRef t (Set ErgveinNodeAddr))
  -- | Get reference to the minimal number of active urls. Internal
  getActiveUrlsNumRef :: m (ExternalRef t Int)
  -- | Get num reference. Internal
  getRequiredUrlNumRef :: m (ExternalRef t (Int, Int))
  -- | Get request timeout ref
  getRequestTimeoutRef :: m (ExternalRef t NominalDiffTime)
  -- | Get indexer request event
  getIndexReqSelector :: m (IndexReqSelector t)
  -- | Get indexer request trigger
  getIndexReqFire :: m (Map ErgveinNodeAddr IndexerMsg -> IO ())
  -- | Get activation event and trigger
  getActivationEF :: m (Event t [ErgveinNodeAddr], [ErgveinNodeAddr] -> IO ())

-- | Get deactivated urls dynamic
getArchivedUrlsD :: MonadClient t m => m (Dynamic t (Set ErgveinNodeAddr))
getArchivedUrlsD = externalRefDynamic =<< getArchivedAddrsRef
-- | Get deactivated urls dynamic
getInactiveUrlsD :: MonadClient t m => m (Dynamic t (Set ErgveinNodeAddr))
getInactiveUrlsD = externalRefDynamic =<< getInactiveAddrsRef

-- | Activate an URL
activateURL :: (MonadClient t m, MonadSettings t m) => Event t ErgveinNodeAddr -> m (Event t ())
activateURL addrE = do
  (_, f)    <- getActivationEF
  iaRef     <- getInactiveAddrsRef
  acrhRef   <- getArchivedAddrsRef
  setRef    <- getSettingsRef
  activsRef <- getActiveAddrsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- modifyExternalRef activsRef $ \as ->
      let as' = S.insert url as in  (as', S.toList as')
    f [url]
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveAddrs       = acs
        , settingsDeactivatedAddrs  = ias
        , settingsArchivedAddrs     = ars
        }
      in (s', s')
    storeSettings s
    fire ()

-- | Activate an URL
activateURLList :: (MonadClient t m, MonadSettings t m) => Event t [ErgveinNodeAddr] -> m (Event t ())
activateURLList addrE = do
  (_, f)    <- getActivationEF
  iaRef     <- getInactiveAddrsRef
  acrhRef   <- getArchivedAddrsRef
  setRef    <- getSettingsRef
  activsRef <- getActiveAddrsRef
  performEventAsync $ ffor addrE $ \urls fire -> void $ liftIO $ forkOnOther $ do
    let urls' = S.fromList urls
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.difference us urls' in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.difference as urls' in  (as', S.toList as')
    acs <- modifyExternalRef activsRef $ \as ->
      let as' = S.union urls' as in  (as', S.toList as')
    f urls
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveAddrs      = acs
        , settingsDeactivatedAddrs = ias
        , settingsArchivedAddrs    = ars
        }
      in (s', s')
    storeSettings s
    fire ()

-- | It is really important to wait until indexer performs deinitialization before deleting it from dynamic collections
closeAndWait :: MonadClient t m => Event t ErgveinNodeAddr -> m (Event t ErgveinNodeAddr)
closeAndWait urlE = do
  req      <- getIndexReqFire
  connsRef <- getActiveConnsRef
  closedEE <- performEvent $ ffor urlE $ \url -> do
    let sa = url
    liftIO $ req $ M.singleton sa IndexerClose
    mconn <- fmap (M.lookup sa) $ readExternalRef connsRef
    pure $ case mconn of
      Nothing -> never
      Just conn -> url <$ indexConClosedE conn
  switchDyn <$> holdDyn never closedEE

-- | Deactivate an URL
deactivateURL :: (MonadClient t m, MonadSettings t m) => Event t ErgveinNodeAddr -> m (Event t ())
deactivateURL addrE = do
  iaRef     <- getInactiveAddrsRef
  setRef    <- getSettingsRef
  activsRef <- getActiveAddrsRef

  closedE <- closeAndWait addrE
  performFork $ ffor closedE $ \url -> void $ do
    acs <- modifyExternalRef activsRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    ias <- modifyExternalRef iaRef  $ \us ->
      let us' = S.insert url us in (us', S.toList us')
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveAddrs  = acs
        , settingsDeactivatedAddrs = ias
        }
      in (s', s')
    storeSettings s

-- | Forget an url
forgetURL :: (MonadClient t m, MonadSettings t m) => Event t ErgveinNodeAddr -> m (Event t ())
forgetURL addrE = do
  iaRef     <- getInactiveAddrsRef
  acrhRef   <- getArchivedAddrsRef
  setRef    <- getSettingsRef
  activsRef <- getActiveAddrsRef

  closedE <- closeAndWait addrE
  performFork $ ffor closedE $ \url -> void $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- modifyExternalRef activsRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')

    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveAddrs  = acs
        , settingsDeactivatedAddrs = ias
        , settingsArchivedAddrs = ars
        }
      in (s', s')
    storeSettings s

broadcastIndexerMessage :: (MonadClient t m) => Event t IndexerMsg -> m ()
broadcastIndexerMessage reqE = do
  connsRef <- getActiveConnsRef
  fire <- getIndexReqFire
  performEvent_ $ ffor reqE $ \req -> do
    cm <- readExternalRef connsRef
    liftIO $ fire $ req <$ cm

requestIndexerWhenOpen :: MonadClient t m => IndexerConnection t -> Message -> m (Event t ())
requestIndexerWhenOpen IndexerConnection{..} msg = do
  fire  <- getIndexReqFire
  initE <- fmap (gate (current indexConIsUp)) $ getPostBuild
  let reqE = leftmost [initE, indexConOpensE]
  performEvent $ ffor reqE $ const $
    liftIO $ fire $ M.singleton indexConName $ IndexerMsg msg

requestSpecificIndexer :: MonadClient t m => Event t (ErgveinNodeAddr, Message) -> m (Event t Message)
requestSpecificIndexer saMsgE = do
  connsRef <- getActiveConnsRef
  fireReq  <- getIndexReqFire
  mrespE <- performFork $ ffor saMsgE $ \(sa, req) -> do
    mcon <- fmap (M.lookup sa) $ readExternalRef connsRef
    case mcon of
      Nothing -> pure Nothing
      Just con -> do
        liftIO $ fireReq $ M.singleton sa $ IndexerMsg req
        pure $ Just $ indexConRespE con
  switchHold never $ fmapMaybe id mrespE

indexerPingerWidget :: MonadClient t m
  => ErgveinNodeAddr                       -- Which indexer to ping
  -> Event t ()                     -- Manual refresh event
  -> m (Dynamic t NominalDiffTime)  -- Dynamic with the latency. Starting value 0
indexerPingerWidget addr refrE = do
  connmD  <- holdUniqDynBy eq =<< pure . fmap (M.lookup addr) =<< externalRefDynamic =<< getActiveConnsRef
  fmap join $ networkHoldDyn $ ffor connmD $ \case
    Nothing -> pure $ pure 0
    Just conn -> indexerConnPingerWidget conn refrE
  where
    eq :: Maybe (IndexerConnection t) -> Maybe (IndexerConnection t) -> Bool
    eq = (==) `on` (fmap indexConAddr)

indexerConnPingerWidget :: MonadClient t m
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

indexersAverageLatencyWidget :: forall t m . MonadClient t m => Event t () -> m (Dynamic t NominalDiffTime)
indexersAverageLatencyWidget = (fmap . fmap) snd . indexersAverageLatNumWidget

indexersAverageLatNumWidget :: forall t m . MonadClient t m => Event t () -> m (Dynamic t (Int, NominalDiffTime))
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

-- | Watch after status updates and save it to separate map in env to display it when connection id down
indexerStatusUpdater :: forall t m . MonadClient t m => IndexerConnection t -> m ()
indexerStatusUpdater IndexerConnection{..} = do
  e <- updatedWithInit =<< holdUniqDyn indexConStatus
  r <- getStatusConnsRef
  performEvent_ $ ffor e $ \status -> do
    modifyExternalRef r $ \m -> (M.insert indexConName status m, ())

-- | Get cached status of indexer even it is disconnected
indexerLastStatus :: forall t m . MonadClient t m => ErgveinNodeAddr -> m (Dynamic t (Maybe IndexerStatus))
indexerLastStatus addr = do
  md <- externalRefDynamic =<< getStatusConnsRef
  pure $ M.lookup addr <$> md
