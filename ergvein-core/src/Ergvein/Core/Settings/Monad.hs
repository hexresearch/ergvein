module Ergvein.Core.Settings.Monad(
    MonadHasSettingsConstr
  , MonadHasSettings(..)
  , getSettings
  , getSettingsD
  , updateSettings
  , modifySettings
  , getSocksConf
  , getProxyConf
  ) where

import Control.Monad.Reader
import Data.Aeson.Types
import Ergvein.Core.Settings.Types
import Reflex
import Reflex.ExternalRef
import Reflex.Localize.Language

import qualified Network.Socks5 as S5

type MonadHasSettingsConstr t m = (
    PerformEvent t m
  , MonadHold t m
  , MonadIO m
  , MonadIO (Performable m)
  , ToJSON Language
  )

class MonadHasSettingsConstr t m => MonadHasSettings t m where
  -- | Get settings ref
  getSettingsRef :: m (ExternalRef t Settings)

instance MonadHasSettingsConstr t m => MonadHasSettings t (ReaderT (ExternalRef t Settings) m) where
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
