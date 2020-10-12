{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Monad.Prim
  (
    MonadBaseConstr
  , MonadAlertPoster(..)
  , AlertType(..)
  , alertTypeToSeverity
  , AlertInfo(..)
  , MonadEgvLogger(..)
  , MonadHasSettings(..)
  -- * Frontend-wide types
  , IndexerInfo(..)
  , PeerScanInfoMap
  , NamedSockAddr(..)
  , getSettings
  , getSettingsD
  , updateSettings
  , modifySettings
  , getDnsList
  , mkResolvSeed
  , resolveSeed
  , initialIndexers
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time(UTCTime, NominalDiffTime)
import Foreign.JavaScript.TH (WithJSContextSingleton)
import Language.Javascript.JSaddle
import Network.DNS
import Network.Socket (HostName, SockAddr)
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Host.Class
import Reflex.Localize
import Reflex.Spider.Internal (SpiderHostFrame, Global)

import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Version

import qualified Reflex.Profiled as RP
import qualified Control.Monad.Fail as F
import qualified Data.Set as S

-- | Type classes that we need from reflex-dom itself.
type MonadBaseConstr t m = (MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadUnliftIO (Performable m)
  , MonadSample t (Performable m)
  , MonadJSM (Performable m)
  , F.MonadFail (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , HasJSContext m
  , HasJSContext (Performable m)
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , MonadRandom (Performable m)
  , PlatformNatives
  , MonadReflexCreateTrigger t m
  )

-- ===========================================================================
--           Monad HasSettings. Gives access to Settings
-- ===========================================================================

class MonadBaseConstr t m => MonadHasSettings t m where
  -- | Get settings ref
  getSettingsRef :: m (ExternalRef t Settings)

instance MonadBaseConstr t m => MonadHasSettings t (ReaderT (ExternalRef t Settings) m) where
  getSettingsRef = ask
  {-# INLINE getSettingsRef #-}

-- | Get current settings
getSettings :: MonadHasSettings t m => m Settings
getSettings = readExternalRef =<< getSettingsRef
{-# INLINE getSettings #-}

-- | Get current settings dynamic
getSettingsD :: MonadHasSettings t m => m (Dynamic t Settings)
getSettingsD = externalRefDynamic =<< getSettingsRef
{-# INLINE getSettingsD #-}

-- | Update app's settings. Sets settings to provided value and stores them
updateSettings :: MonadHasSettings t m => Event t Settings -> m (Event t ())
updateSettings setE = do
  settingsRef <- getSettingsRef
  performEvent $ ffor setE $ \s -> do
    writeExternalRef settingsRef s
    storeSettings s
{-# INLINE updateSettings #-}

-- | Update app's settings. Sets settings to provided value and stores them
modifySettings :: MonadHasSettings t m => Event t (Settings -> Settings) -> m (Event t ())
modifySettings setE = do
  settingsRef <- getSettingsRef
  performEvent $ ffor setE $ \f -> do
    storeSettings =<< modifyExternalRef settingsRef (\s -> let s' = f s in (s',s'))
{-# INLINE modifySettings #-}

getDnsList :: MonadHasSettings t m => m [HostName]
getDnsList = fmap (S.toList . settingsDns) $ readExternalRef =<< getSettingsRef
{-# INLINE getDnsList #-}

mkResolvSeed :: MonadHasSettings t m => m ResolvSeed
mkResolvSeed = do
  dns <- getDnsList
  liftIO $ resolveSeed dns
{-# INLINE mkResolvSeed #-}

resolveSeed :: [HostName] -> IO ResolvSeed
resolveSeed dns = makeResolvSeed defaultResolvConf {
      resolvInfo = if null dns
        then resolvInfo defaultResolvConf -- resolve via "/etc/resolv.conf" by default
        else RCHostNames dns
    , resolvConcurrent = True
    }
{-# INLINE resolveSeed #-}

initialIndexers :: IO [Text]
initialIndexers = do
  resolvInfo <- makeResolvSeed defaultResolvConf {
      resolvInfo = RCHostNames $ S.toList $ defaultDns
    , resolvConcurrent = True
    }
  tryDNS <- getDNS resolvInfo seedList
  pure $ fromMaybe defaultIndexers tryDNS


-- ===========================================================================
--           Monad EgvLogger. Implements Ervgein's logging
-- ===========================================================================

class MonadBaseConstr t m => MonadEgvLogger t m where
  -- | Internal getting of logs event and trigger
  getLogsTrigger :: m (Event t LogEntry, LogEntry -> IO ())
  -- | Get internal ref fo namespaces
  getLogsNameSpacesRef :: m (ExternalRef t [Text])

-- ===========================================================================
--           Monad Alert Poster. Implements rendering of alerts
-- ===========================================================================

-- | Different styles of alerts (including success or info messages)
data AlertType =
    AlertTypeInfo
  | AlertTypePrimary
  | AlertTypeSecondary
  | AlertTypeWarn
  | AlertTypeSuccess
  | AlertTypeFail
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Transformation from alert types to log entry types
alertTypeToSeverity :: AlertType -> LogSeverity
alertTypeToSeverity et = case et of
  AlertTypeInfo       -> LogInfo
  AlertTypePrimary    -> LogInfo
  AlertTypeSecondary  -> LogInfo
  AlertTypeWarn       -> LogWarning
  AlertTypeSuccess    -> LogInfo
  AlertTypeFail       -> LogError

-- | All info that is required to draw alert message to user
data AlertInfo = forall a . (LocalizedPrint a, Eq a) =>  AlertInfo {
  alertType       :: !AlertType -- ^ Style of message
, alertTimeout    :: !Double    -- ^ Amount of seconds the message should be shown
, alertNameSpace  :: ![Text]    -- ^ Optional name space for logs
, alertTime       :: !UTCTime   -- ^ Time of alert
, alertDoLog      :: !Bool      -- ^ Whether to log the alert or not
, alertMessage    :: !a         -- ^ Message to display
}

-- | Allows to delegate alert displaying to another widget without coupling with it
class MonadBaseConstr t m => MonadAlertPoster t m | m -> t where
  -- | Add timed alert to queue of alerts to be displayed with special widget
  postAlert :: Event t AlertInfo -> m ()
  -- | Fires when new alert arrives from 'postAlert'
  newAlertEvent :: m (Event t AlertInfo)
  -- | Get alert's event and trigger. Internal
  getAlertEventFire :: m (Event t AlertInfo, AlertInfo -> IO ())


-- ===========================================================================
--    Frontend-wide types
-- ===========================================================================

type PeerScanInfoMap = Map Currency (Maybe BlockHeight, BlockHeight) -- (scanned, actual)

data IndexerInfo = IndexerInfo {
  indInfoHeights :: PeerScanInfoMap
, indInfoLatency :: NominalDiffTime
} deriving (Show, Eq)

data NamedSockAddr = NamedSockAddr {
  namedAddrName :: Text
, namedAddrSock :: SockAddr
} deriving (Eq, Ord)


-- ===========================================================================
--    Helper instances for base monad
-- ===========================================================================

instance MonadRandom m => MonadRandom (ReaderT e m) where
  getRandomBytes = lift . getRandomBytes
  {-# INLINE getRandomBytes #-}

instance MonadRandom (WithJSContextSingleton x (SpiderHostFrame Global)) where
  getRandomBytes = liftIO . getRandomBytes

instance MonadRandom (WithJSContextSingleton x (RP.ProfiledM (SpiderHostFrame Global))) where
  getRandomBytes = liftIO . getRandomBytes

instance F.MonadFail (WithJSContextSingleton x (SpiderHostFrame Global)) where
  fail = liftIO . F.fail

instance F.MonadFail (WithJSContextSingleton x (RP.ProfiledM (SpiderHostFrame Global))) where
  fail = liftIO . F.fail
