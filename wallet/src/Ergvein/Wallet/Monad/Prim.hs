{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Monad.Prim
  (
    MonadBaseConstr
  , MonadAlertPoster(..)
  , AlertType(..)
  , alertTypeToSeverity
  , MonadNativeLogger(..)
  , MonadHasSettings(..)
  , MonadHasMain(..)
  -- * Frontend-wide types
  , IndexerInfo(..)
  , PeerScanInfoMap
  , getSettings
  , getSettingsD
  , updateSettings
  , modifySettings
  , getSocksConf
  , getProxyConf
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time(UTCTime, NominalDiffTime)
import Foreign.JavaScript.TH (WithJSContextSingleton)
import Language.Javascript.JSaddle
import Network.DNS
import Network.Socket (HostName, SockAddr)
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.ExternalRef
import Reflex.Host.Class
import Reflex.Localize
import Reflex.Spider.Internal (SpiderHostFrame, Global)

import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Sepulcas.Alert
import Sepulcas.Monad
import Sepulcas.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings

import qualified Control.Monad.Fail as F
import qualified Network.Socks5 as S5
import qualified Reflex.Profiled as RP
import qualified Data.Set as S

-- | Type classes that we need from reflex-dom itself.
type MonadBaseConstr t m = (MonadReflex t m, MonadRandom (Performable m))

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

getSocksConf :: MonadHasSettings t m => m (Dynamic t (Maybe S5.SocksConf))
getSocksConf = fmap (fmap toSocksProxy . settingsSocksProxy) <$> getSettingsD
{-# INLINE getSocksConf #-}

getProxyConf :: MonadHasSettings t m => m (Dynamic t (Maybe SocksConf))
getProxyConf = fmap settingsSocksProxy <$> getSettingsD
{-# INLINE getProxyConf #-}

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

-- | This env is used for seed resolvment
type SeedResolvEnv t = (Chan (IO ()), ExternalRef t Settings)

instance MonadBaseConstr t m => MonadHasSettings t (ReaderT (SeedResolvEnv t) m) where
  getSettingsRef = asks snd
  {-# INLINE getSettingsRef #-}

instance MonadRandom m => MonadRandom (ReaderT e m) where
  getRandomBytes = lift . getRandomBytes
  {-# INLINE getRandomBytes #-}

instance MonadRandom (WithJSContextSingleton x (SpiderHostFrame Global)) where
  getRandomBytes = liftIO . getRandomBytes

instance MonadRandom (WithJSContextSingleton x (RP.ProfiledM (SpiderHostFrame Global))) where
  getRandomBytes = liftIO . getRandomBytes
