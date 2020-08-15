module Ergvein.Wallet.Monad.Client (
    MonadIndexClient(..)
  , IndexerConnection(..)
  , IndexerMsg(..)
  , IndexReqSelector
  , getArchivedUrlsD
  , getInactiveUrlsD
  , activateURL
  , deactivateURL
  , forgetURL
  , broadcastIndexerMessage
  , requestRandomIndexer
  , requestSpecificIndexer
  , indexerPingerWidget
  , indexerConnPingerWidget
  , indexersAverageLatencyWidget
  , indexersAverageLatNumWidget
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time(NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word64)
import Network.HTTP.Client hiding (Proxy)
import Reflex
import Reflex.ExternalRef

import Network.Socket (SockAddr)
import Data.Functor.Misc (Const2(..))

import Ergvein.Index.Protocol.Types (Message(..), ScanBlock)
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

data IndexerConnection t = IndexerConnection {
  indexConAddr :: !SockAddr
, indexConClosedE :: !(Event t ())
, indexConOpensE :: !(Event t ())
, indexConIsUp :: !(Dynamic t Bool)
, indexConRespE :: !(Event t Message)
}

data IndexerMsg = IndexerClose | IndexerRestart | IndexerMsg Message

type IndexReqSelector t = EventSelector t (Const2 SockAddr IndexerMsg)

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

class MonadBaseConstr t m => MonadIndexClient t m | m -> t where
  -- | Get passive urls' reference. Internal
  getArchivedUrlsRef :: m (ExternalRef t (Set SockAddr))
  -- | Internal method to get reference to indexers
  getActiveConnsRef :: m (ExternalRef t (Map SockAddr (IndexerConnection t)))
  -- | Get deactivated urls' reference. Internal
  getInactiveUrlsRef :: m (ExternalRef t (Set SockAddr))
  -- | Get reference to the minimal number of active urls. Internal
  getActiveUrlsNumRef :: m (ExternalRef t Int)
  -- | Get num reference. Internal
  getRequiredUrlNumRef :: m (ExternalRef t (Int, Int))
  -- | Get request timeout ref
  getRequestTimeoutRef :: m (ExternalRef t NominalDiffTime)
  -- | Get indexer request event
  getIndexReqSelector :: m (IndexReqSelector t)
  -- | Get indexer request trigger
  getIndexReqFire :: m (Map SockAddr IndexerMsg -> IO ())
  -- | Get activation event and trigger
  getActivationEF :: m (Event t SockAddr, SockAddr -> IO ())

-- | Get deactivated urls dynamic
getArchivedUrlsD :: MonadIndexClient t m => m (Dynamic t (Set SockAddr))
getArchivedUrlsD = externalRefDynamic =<< getArchivedUrlsRef
-- | Get deactivated urls dynamic
getInactiveUrlsD :: MonadIndexClient t m => m (Dynamic t (Set SockAddr))
getInactiveUrlsD = externalRefDynamic =<< getInactiveUrlsRef

-- | Activate an URL
activateURL :: (MonadIndexClient t m, MonadHasSettings t m) => Event t SockAddr -> m (Event t ())
activateURL addrE = do
  (_, f)    <- getActivationEF
  iaRef     <- getInactiveUrlsRef
  acrhRef   <- getArchivedUrlsRef
  setRef    <- getSettingsRef
  connsRef  <- getActiveConnsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- fmap M.keys $ readExternalRef connsRef
    f url
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveSockAddrs  = L.nub $ url:acs
        , settingsDeactivSockAddrs = ias
        , settingsPassiveSockAddrs = ars
        }
      in (s', s')
    storeSettings s
    fire ()

-- | Deactivate an URL
deactivateURL :: (MonadIndexClient t m, MonadHasSettings t m) => Event t SockAddr -> m (Event t ())
deactivateURL addrE = do
  req       <- getIndexReqFire
  iaRef     <- getInactiveUrlsRef
  setRef    <- getSettingsRef
  connsRef  <- getActiveConnsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    acs <- fmap M.keys $ readExternalRef connsRef
    ias <- modifyExternalRef iaRef  $ \us ->
      let us' = S.insert url us in (us', S.toList us')

    req $ M.singleton url IndexerClose
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveSockAddrs  = L.filter (/= url) acs
        , settingsDeactivSockAddrs = ias
        }
      in (s', s')
    storeSettings s
    fire ()

-- | Forget an url
forgetURL :: (MonadIndexClient t m, MonadHasSettings t m) => Event t SockAddr -> m (Event t ())
forgetURL addrE = do
  req       <- getIndexReqFire
  iaRef     <- getInactiveUrlsRef
  acrhRef   <- getArchivedUrlsRef
  setRef    <- getSettingsRef
  connsRef  <- getActiveConnsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- fmap M.keys $ readExternalRef connsRef
    req $ M.singleton url IndexerClose
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveSockAddrs  = L.filter (/= url) acs
        , settingsDeactivSockAddrs = ias
        , settingsPassiveSockAddrs = ars
        }
      in (s', s')
    storeSettings s
    fire ()

broadcastIndexerMessage :: (MonadIndexClient t m) => Event t IndexerMsg -> m ()
broadcastIndexerMessage reqE = do
  connsRef  <- getActiveConnsRef
  fire <- getIndexReqFire
  performEvent_ $ ffor reqE $ \req -> do
    cm <- readExternalRef connsRef
    liftIO $ fire $ req <$ cm

requestRandomIndexer :: MonadIndexClient t m => Event t Message -> m (Event t Message)
requestRandomIndexer reqE = do
  connsRef  <- getActiveConnsRef
  fireReq   <- getIndexReqFire
  preE <- performEvent $ ffor reqE $ \req -> do
    cons <- fmap M.toList $ readExternalRef connsRef
    case cons of
      [] -> pure $ Nothing
      _ -> do
        i <- liftIO $ randomRIO (0, length cons - 1)
        let (sa, conn) = cons!!i
        liftIO $ fireReq $ M.singleton sa $ IndexerMsg req
        pure $ Just $ indexConRespE conn
  switchHold never $ fmapMaybe id preE

requestSpecificIndexer :: MonadIndexClient t m => Event t (SockAddr, Message) -> m (Event t Message)
requestSpecificIndexer saMsgE = do
  connsRef  <- getActiveConnsRef
  fireReq   <- getIndexReqFire
  mrespE <- performFork $ ffor saMsgE $ \(sa, req) -> do
    mcon <- fmap (M.lookup sa) $ readExternalRef connsRef
    case mcon of
      Nothing -> pure Nothing
      Just con -> do
        liftIO $ fireReq $ M.singleton sa $ IndexerMsg req
        pure $ Just $ indexConRespE con
  switchHold never $ fmapMaybe id mrespE

data PingStore = PingSent (Word64, UTCTime) | PongRec (Word64, UTCTime) | LatRes NominalDiffTime

indexerPingerWidget :: MonadIndexClient t m
  => SockAddr                       -- Which indexer to ping
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
    fireReq $ M.singleton indexConAddr $ (IndexerMsg $ PingMsg p)
    pure (p,t)
  pingD <- holdDyn Nothing $ Just <$> pingE
  pongE <- performFork $ ffor indexConRespE $ \case
    PongMsg p -> do
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
    liftIO $ fireReq $ (IndexerMsg $ PingMsg p) <$ conns
    pure (p,t)
  pingD <- holdDyn Nothing $ Just <$> pingE
  pongsD <- list connsD $ \connD -> do
    let respE = switchDyn $ indexConRespE <$> connD
    pongE <- performFork $ ffor respE $ \case
      PongMsg p -> do
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
