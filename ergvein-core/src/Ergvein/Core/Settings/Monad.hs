module Ergvein.Core.Settings.Monad(
    MonadSettingsConstr
  , MonadSettings(..)
  , getSettings
  , getSettingsD
  , updateSettings
  , modifySettings
  , getSocksConf
  , getProxyConf
  , getFiatBalanceSettings
  , getFiatRateSettings
  , module Ergvein.Core.Settings.Types
  ) where

import Control.Monad.Fail
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson.Types
import Ergvein.Core.Settings.Types
import Ergvein.Types.Currency
import Reflex
import Reflex.ExternalRef
import Reflex.Localize.Language
import Sepulcas.Native

import qualified Network.Socks5 as S5

type MonadSettingsConstr t m = (
    Adjustable t m
  , MonadFail (Performable m)
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , MonadIO m
  , MonadUnliftIO (Performable m)
  , MonadSample t (Performable m)
  , PerformEvent t m
  , PlatformNatives
  , PostBuild t m
  , ToJSON Language
  , TriggerEvent t m
  )

class MonadSettingsConstr t m => MonadSettings t m where
  -- | Get settings ref
  getSettingsRef :: m (ExternalRef t Settings)

instance MonadSettingsConstr t m => MonadSettings t (ReaderT (ExternalRef t Settings) m) where
  getSettingsRef = ask
  {-# INLINE getSettingsRef #-}

-- | Get current settings
getSettings :: MonadSettings t m => m Settings
getSettings = readExternalRef =<< getSettingsRef
{-# INLINE getSettings #-}

-- | Get current settings dynamic
getSettingsD :: MonadSettings t m => m (Dynamic t Settings)
getSettingsD = externalRefDynamic =<< getSettingsRef
{-# INLINE getSettingsD #-}

-- | Update app's settings. Sets settings to provided value and stores them
updateSettings :: MonadSettings t m => Event t Settings -> m (Event t ())
updateSettings setE = do
  settingsRef <- getSettingsRef
  performEvent $ ffor setE $ \s -> do
    writeExternalRef settingsRef s
    storeSettings s
{-# INLINE updateSettings #-}

-- | Update app's settings. Sets settings to provided value and stores them
modifySettings :: MonadSettings t m => Event t (Settings -> Settings) -> m (Event t ())
modifySettings setE = do
  settingsRef <- getSettingsRef
  performEvent $ ffor setE $ \f -> do
    storeSettings =<< modifyExternalRef settingsRef (\s -> let s' = f s in (s',s'))
{-# INLINE modifySettings #-}

getSocksConf :: MonadSettings t m => m (Dynamic t (Maybe S5.SocksConf))
getSocksConf = fmap (fmap toSocksProxy . settingsSocksProxy) <$> getSettingsD
{-# INLINE getSocksConf #-}

getProxyConf :: MonadSettings t m => m (Dynamic t (Maybe SocksConf))
getProxyConf = fmap settingsSocksProxy <$> getSettingsD
{-# INLINE getProxyConf #-}

getFiatBalanceSettings :: MonadSettings t m => m (Dynamic t (Maybe Fiat))
getFiatBalanceSettings = do
  settingsD <- getSettingsD
  mFiatD <- holdUniqDyn $ fmap settingsFiatCurr settingsD
  mBalanceD <- holdUniqDyn $ fmap settingsShowFiatBalance settingsD
  pure $ ffor2 mFiatD mBalanceD (\fiatCurrency showBalance -> if showBalance then Just fiatCurrency else Nothing)
{-# INLINE getFiatBalanceSettings #-}

getFiatRateSettings :: MonadSettings t m => m (Dynamic t (Maybe Fiat))
getFiatRateSettings = do
  settingsD <- getSettingsD
  mFiatD <- holdUniqDyn $ fmap settingsFiatCurr settingsD
  mRateD <- holdUniqDyn $ fmap settingsShowFiatRate settingsD
  pure $ ffor2 mFiatD mRateD (\fiatCurrency showRate -> if showRate then Just fiatCurrency else Nothing)
{-# INLINE getFiatRateSettings #-}
