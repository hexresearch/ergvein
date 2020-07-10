module Ergvein.Wallet.Monad.Client (
    MonadClient(..)
  , activateURL
  , deactivateURL
  , forgetURL
  , getArchivedUrlsD
  , getInactiveUrlsD
  , getIndexerInfoD
  , pingIndexer
  , pingIndexerIO
  , refreshIndexerInfo
  , restoreDefaultIndexers
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time(NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.HTTP.Client hiding (Proxy)
import Reflex
import Reflex.ExternalRef
import Servant.Client(BaseUrl, showBaseUrl)

import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

class (
    MonadBaseConstr t m
  , HasClientManager m
  , HasClientManager (Performable m)
  ) => MonadClient t m | m -> t where
  -- | Get passive urls' reference. Internal
  getArchivedUrlsRef :: m (ExternalRef t (Set BaseUrl))
  -- | Internal method to get reference to indexers
  getActiveUrlsRef :: m (ExternalRef t (Map BaseUrl (Maybe IndexerInfo)))
  -- | Get deactivated urls' reference. Internal
  getInactiveUrlsRef :: m (ExternalRef t (Set BaseUrl))
  -- | Get reference to the minimal number of active urls. Internal
  getActiveUrlsNumRef :: m (ExternalRef t Int)
  -- | Get num reference. Internal
  getRequiredUrlNumRef :: m (ExternalRef t (Int, Int))
  -- | Get request timeout ref
  getRequestTimeoutRef :: m (ExternalRef t NominalDiffTime)
  -- | Get event and trigger for indexer info refresher
  getIndexerInfoEF  :: m (Event t (), IO ())

-- ===========================================================================
--    Monad Client helpers
-- ===========================================================================

-- | Get deactivated urls dynamic
getArchivedUrlsD :: MonadClient t m => m (Dynamic t (Set BaseUrl))
getArchivedUrlsD = externalRefDynamic =<< getArchivedUrlsRef
-- | Get deactivated urls dynamic
getInactiveUrlsD :: MonadClient t m => m (Dynamic t (Set BaseUrl))
getInactiveUrlsD = externalRefDynamic =<< getInactiveUrlsRef
-- | Get a dynamic with indexer info map
getIndexerInfoD :: MonadClient t m => m (Dynamic t (Map BaseUrl (Maybe IndexerInfo)))
getIndexerInfoD = externalRefDynamic =<< getActiveUrlsRef

-- | Request stats from a specific URL
pingIndexer :: MonadClient t m => Event t BaseUrl -> m (Event t (BaseUrl, Maybe IndexerInfo))
pingIndexer urlE = performFork $ ffor urlE $ \url -> do
  mng <- getClientManager
  pingIndexerIO mng url
{-# INLINE pingIndexer #-}

-- | IO pinger. This part is used multiple times, so it's separated
pingIndexerIO :: (MonadIO m, PlatformNatives) => Manager -> BaseUrl -> m (BaseUrl, Maybe IndexerInfo)
pingIndexerIO mng url = liftIO $ do
  t0 <- getCurrentTime
  res <- runReaderT (getInfoEndpoint url ()) mng
  t1 <- getCurrentTime
  case res of
    Left err -> do
      logWrite $ "[pingIndexerIO][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
      pure $ (url, Nothing)
    Right (InfoResponse vals) -> let
      curmap = M.fromList $ fmap (\(ScanProgressItem cur sh ah) -> (cur, (sh, ah))) vals
      in pure $ (url, Just $ IndexerInfo curmap $ diffUTCTime t1 t0)

-- | Call indexer info to be refreshed
refreshIndexerInfo :: MonadClient t m => Event t () -> m ()
refreshIndexerInfo e = do
  fire <- fmap snd getIndexerInfoEF
  performEvent_ $ (liftIO fire) <$ e
{-# INLINE refreshIndexerInfo #-}

-- | Restore default indexers from Settings.hs
restoreDefaultIndexers :: (MonadClient t m, MonadHasSettings t m) => Event t () -> m (Event t ())
restoreDefaultIndexers reqE = do
  actRef  <- getActiveUrlsRef
  iaRef   <- getInactiveUrlsRef
  acrhRef <- getArchivedUrlsRef
  setRef  <- getSettingsRef
  let defSet = S.fromList defaultIndexers
  performEvent $ ffor reqE $ const $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = us `S.difference` defSet in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = as `S.difference` defSet in  (as', S.toList as')
    acs <- modifyExternalRef actRef $ \as ->
      let as' = L.foldl' (\m u -> M.insert u Nothing m) as defaultIndexers
      in (as', M.keys as')
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveUrls      = acs
        , settingsDeactivatedUrls = ias
        , settingsPassiveUrls     = ars
        }
      in (s', s')
    storeSettings s
    pure ()

-- | Activate an URL
activateURL :: (MonadClient t m, MonadHasSettings t m) => Event t BaseUrl -> m (Event t ())
activateURL urlE = do
  actRef  <- getActiveUrlsRef
  iaRef   <- getInactiveUrlsRef
  acrhRef <- getArchivedUrlsRef
  setRef  <- getSettingsRef
  performFork $ ffor urlE $ \url -> do
    mng <- getClientManager
    res <- pingIndexerIO mng url
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- modifyExternalRef actRef $ \as ->
      let as' = uncurry M.insert res as in (as', M.keys as')
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveUrls      = acs
        , settingsDeactivatedUrls = ias
        , settingsPassiveUrls     = ars
        }
      in (s', s')
    storeSettings s
    pure ()

-- | Deactivate an URL
deactivateURL :: (MonadClient t m, MonadHasSettings t m) => Event t BaseUrl -> m (Event t ())
deactivateURL urlE = do
  actRef  <- getActiveUrlsRef
  iaRef   <- getInactiveUrlsRef
  setRef  <- getSettingsRef
  performEventAsync $ ffor urlE $ \url fire -> void $ liftIO $ forkOnOther $ do
    acs <- modifyExternalRef actRef $ \as ->
      let as' = M.delete url as in (as', M.keys as')
    ias <- modifyExternalRef iaRef  $ \us ->
      let us' = S.insert url us in (us', S.toList us')
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveUrls      = acs
        , settingsDeactivatedUrls = ias
        }
      in (s', s')
    storeSettings s
    fire ()

-- | Forget an url
forgetURL :: (MonadClient t m, MonadHasSettings t m) => Event t BaseUrl -> m (Event t ())
forgetURL urlE = do
  actRef  <- getActiveUrlsRef
  iaRef   <- getInactiveUrlsRef
  acrhRef <- getArchivedUrlsRef
  setRef  <- getSettingsRef
  performEvent $ ffor urlE $ \url -> do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- modifyExternalRef actRef $ \as ->
      let as' = M.delete url as in (as', M.keys as')
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveUrls      = acs
        , settingsDeactivatedUrls = ias
        , settingsPassiveUrls     = ars
        }
      in (s', s')
    storeSettings s

-- ===========================================================================
--    Misc
-- ===========================================================================

instance MonadIO m => HasClientManager (ReaderT Manager m) where
  getClientManager = ask
