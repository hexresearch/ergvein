{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Settings.Types(
    Settings(..)
  , BtcSettings(..)
  , CurrencySettings(..)
  , getBtcSettings
  , loadSettings
  , storeSettings
  , defaultSettings
  , defaultIndexers
  , defIndexerPort
  , defaultIndexersNum
  , defaultSeedNodesSource
  , defaultIndexerTimeout
  , defaultActUrlNum
  , ExplorerUrls(..)
  , defaultDns
  , SocksConf(..)
  , torSocks
  , toSocksProxy
  -- * Helpers
  , makeSockAddr
  , parseIP
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (encodeFile)
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Network.Socket (HostName, PortNumber)
import Reflex.Localize.Language
import Sepulcas.Native
import System.Directory

import Ergvein.Aeson
import Ergvein.Core.IP
import Ergvein.Core.Settings.Constants
import Ergvein.Core.Yaml (readYamlEither')
import Ergvein.Lens
import Ergvein.Text
import Ergvein.Types.Currency

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Socks5 as S5
import qualified Data.Set as S

#ifdef ANDROID
import Android.HaskellActivity
#endif

data ExplorerUrls = ExplorerUrls {
  testnetUrl :: !Text
, mainnetUrl :: !Text
} deriving (Eq, Show, Read)

instance ToJSON ExplorerUrls where
  toJSON ExplorerUrls{..} = object [
      "testnetUrl" .= toJSON testnetUrl
    , "mainnetUrl" .= toJSON mainnetUrl
   ]

instance FromJSON ExplorerUrls where
  parseJSON = withObject "ExplorerUrls" $ \o -> do
    testnetUrl          <- o .: "testnetUrl"
    mainnetUrl          <- o .: "mainnetUrl"
    pure ExplorerUrls{..}

btcDefaultExplorerUrls :: ExplorerUrls
btcDefaultExplorerUrls = ExplorerUrls "https://mempool.space/testnet" "https://mempool.space"

data SocksConf = SocksConf {
  socksConfAddr :: !IP
, socksConfPort :: !Int
} deriving (Eq, Show)

instance ToJSON SocksConf where
  toJSON SocksConf{..} = object [
      "address" .= showt socksConfAddr
    , "port" .= socksConfPort
    ]

instance FromJSON SocksConf where
  parseJSON = withObject "SocksConf" $ \o -> do
    addrText <- o .: "address"
    socksConfAddr <- maybe (fail "Cannot parse IP of socks proxy") pure . parseIP $ addrText
    socksConfPort <- o .: "port"
    pure SocksConf{..}

-- | Default tor socks proxy
torSocks :: SocksConf
torSocks = SocksConf "127.0.0.1" 9050

toSocksProxy :: SocksConf -> S5.SocksConf
toSocksProxy (SocksConf a p) = S5.defaultSocksConfFromSockAddr $ makeSockAddr a p

data BtcSettings = BtcSettings {
    btcSettings'explorerUrls     :: !ExplorerUrls
  , btcSettings'sendRbfByDefault :: !Bool
  , btcSettings'units            :: !UnitBTC
  } deriving (Eq, Show, Read)

instance ToJSON BtcSettings where
  toJSON BtcSettings{..} = object [
      "explorerUrls" .= toJSON btcSettings'explorerUrls
    , "sendRbfByDefault" .= toJSON btcSettings'sendRbfByDefault
    , "units" .= toJSON btcSettings'units
    ]

instance FromJSON BtcSettings where
  parseJSON = withObject "BtcSettings" $ \o -> do
    btcSettings'explorerUrls <- o .: "explorerUrls"
    btcSettings'sendRbfByDefault <- o .: "sendRbfByDefault"
    btcSettings'units <- o .: "units"
    pure BtcSettings{..}

defaultBtcSettings :: BtcSettings
defaultBtcSettings = BtcSettings {
    btcSettings'explorerUrls = btcDefaultExplorerUrls
  , btcSettings'sendRbfByDefault = True
  , btcSettings'units = defUnitBTC
}

data CurrencySettings = SettingsBtc !BtcSettings
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''CurrencySettings)

type CurrencySpecificSettings = M.Map Currency CurrencySettings

defaultCurrencySpecificSettings :: CurrencySpecificSettings
defaultCurrencySpecificSettings = M.fromList $ btcDefaultSettings
  where
    btcDefaultSettings  = [(BTC, SettingsBtc defaultBtcSettings)]

getBtcSettings :: Settings -> BtcSettings
getBtcSettings settings = case M.lookup BTC (settingsCurrencySpecific settings) of
  Just (SettingsBtc btcSettings) -> btcSettings
  _ -> defaultBtcSettings

data Settings = Settings {
  settingsLang              :: Language
, settingsStoreDir          :: Text
, settingsConfigPath        :: Text
, settingsReqTimeout        :: NominalDiffTime
, settingsActiveAddrs       :: [Text]
, settingsDeactivatedAddrs  :: [Text]
, settingsArchivedAddrs     :: [Text]
, settingsPortfolio         :: Bool
, settingsFiatCurr          :: Fiat
, settingsShowFiatBalance   :: Bool
, settingsShowFiatRate      :: Bool
, settingsDns               :: S.Set HostName
, settingsSocksProxy        :: Maybe SocksConf
, settingsCurrencySpecific  :: CurrencySpecificSettings
}

deriving instance Eq Language => Eq Settings
deriving instance Show Language => Show Settings

makeLensesWith humbleFields ''Settings

$(deriveJSON defaultOptions ''PortNumber)
$(deriveJSON defaultOptions ''SockAddr)

instance (PlatformNatives, FromJSON Language) => FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    settingsLang              <- o .:  "lang"
    settingsStoreDir          <- o .:  "storeDir"
    settingsConfigPath        <- o .:  "configPath"
    settingsReqTimeout        <- o .:  "reqTimeout"
    settingsActiveAddrs       <- o .:  "activeAddrs"      .!= mempty
    settingsDeactivatedAddrs  <- o .:  "deactivatedAddrs" .!= mempty
    settingsArchivedAddrs     <- o .:  "archivedAddrs"    .!= mempty
    settingsPortfolio         <- o .:? "portfolio"        .!= False
    settingsFiatCurr          <- o .:? "fiatCurr"         .!= USD
    settingsShowFiatBalance   <- o .:? "showFiatBalance"  .!= False
    settingsShowFiatRate      <- o .:? "showFiatRate"     .!= False
    mdns                      <- o .:? "dns"
    settingsSocksProxy        <- o .:? "socksProxy"
    let settingsDns = maybe defaultDns S.fromList mdns
    settingsCurrencySpecific  <- o .:? "currencySpecific" .!= defaultCurrencySpecificSettings
    pure Settings{..}

instance ToJSON Language => ToJSON Settings where
  toJSON Settings{..} = object [
      "lang"              .= toJSON settingsLang
    , "storeDir"          .= toJSON settingsStoreDir
    , "configPath"        .= toJSON settingsConfigPath
    , "reqTimeout"        .= toJSON settingsReqTimeout
    , "activeAddrs"       .= toJSON settingsActiveAddrs
    , "deactivatedAddrs"  .= toJSON settingsDeactivatedAddrs
    , "archivedAddrs"     .= toJSON settingsArchivedAddrs
    , "portfolio"         .= toJSON settingsPortfolio
    , "fiatCurr"          .= toJSON settingsFiatCurr
    , "showFiatBalance"   .= toJSON settingsShowFiatBalance
    , "showFiatRate"      .= toJSON settingsShowFiatRate
    , "dns"               .= toJSON settingsDns
    , "socksProxy"        .= toJSON settingsSocksProxy
    , "currencySpecific"  .= toJSON settingsCurrencySpecific
   ]

defaultSettings :: PlatformNatives => Language -> FilePath -> Settings
defaultSettings deflang home =
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
  in Settings {
        settingsLang              = deflang
      , settingsStoreDir          = pack storePath
      , settingsConfigPath        = pack configPath
      , settingsReqTimeout        = defaultIndexerTimeout
      , settingsPortfolio         = False
      , settingsFiatCurr          = USD
      , settingsShowFiatBalance   = False
      , settingsShowFiatRate      = False
      , settingsActiveAddrs       = []
      , settingsDeactivatedAddrs  = []
      , settingsArchivedAddrs     = []
      , settingsDns               = defaultDns
      , settingsSocksProxy        = Nothing
      , settingsCurrencySpecific  = defaultCurrencySpecificSettings
      }

-- | TODO: Implement some checks to see if the configPath folder is ok to write to
storeSettings :: (MonadIO m, ToJSON Language) => Settings -> m ()
storeSettings s = liftIO $ do
  let configPath = settingsConfigPath s
  createDirectoryIfMissing True $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd "/" configPath
  encodeFile (unpack configPath) s

#ifdef ANDROID
loadSettings :: (MonadIO m, PlatformNatives, FromJSON Language, ToJSON Language) =>  Language -> Maybe FilePath -> m Settings
loadSettings deflang = const $ liftIO $ do
  mpath <- getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let configPath = path <> "/config.yaml"
      ex <- doesFileExist configPath
      cfg <- if not ex
        then pure $ defaultSettings deflang path
        else fmap (either (const $ defaultSettings deflang path) id) $ readYamlEither' configPath
      createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
      encodeFile (unpack $ settingsConfigPath cfg) cfg
      pure cfg

#else
mkDefSettings :: (MonadIO m, PlatformNatives) => Language -> m Settings
mkDefSettings deflang = liftIO $ do
  home <- getHomeDirectory
  putStrLn   "[ WARNING ]: Failed to load config. Reverting to default values: "
  putStrLn $ "Config path: " <> home <> "/.ergvein/config.yaml"
  putStrLn $ "Store  path: " <> home <> "/.ergvein/store"
  putStrLn $ "Language   : English"
  pure $ defaultSettings deflang (home <> "/.ergvein")

loadSettings :: (MonadIO m, PlatformNatives, FromJSON Language, ToJSON Language) => Language -> Maybe FilePath -> m Settings
loadSettings deflang mpath = liftIO $ case mpath of
  Nothing -> do
    home <- getHomeDirectory
    let path = home <> "/.ergvein/config.yaml"
    putStrLn "[ WARNING ]: No path provided. Trying the default: "
    putStrLn path
    loadSettings deflang $ Just path
  Just path -> do
    ex <- doesFileExist path
    cfg <- if not ex
      then mkDefSettings deflang
      else either (const $ mkDefSettings deflang) pure =<< readYamlEither' path
    createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
    encodeFile (unpack $ settingsConfigPath cfg) cfg
    pure cfg
#endif
