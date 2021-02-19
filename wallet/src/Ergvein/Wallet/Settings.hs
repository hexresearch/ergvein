{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , BtcSettings(..)
  , ErgoSettings(..)
  , CurrencySettings(..)
  , getBtcSettings
  , getErgoSettings
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
import Data.Maybe
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Network.Socket (HostName, PortNumber)
import System.Directory
import Network.DNS.Types

import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Node.Constants
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.IP
import Ergvein.Wallet.Language
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Yaml(readYamlEither')

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Socks5 as S5
import qualified Data.Set as S

#ifdef ANDROID
import Android.HaskellActivity
import Ergvein.Wallet.Native
#endif

data ExplorerUrls = ExplorerUrls {
  testnetUrl :: !Text
, mainnetUrl :: !Text
} deriving (Eq, Show, Read)

instance ToJSON ExplorerUrls where
  toJSON ExplorerUrls{..} = object [
      "testnetUrl"  .= toJSON testnetUrl
    , "mainnetUrl"  .= toJSON mainnetUrl
   ]

instance FromJSON ExplorerUrls where
  parseJSON = withObject "ExplorerUrls" $ \o -> do
    testnetUrl          <- o .: "testnetUrl"
    mainnetUrl          <- o .: "mainnetUrl"
    pure ExplorerUrls{..}

btcDefaultExplorerUrls :: ExplorerUrls
btcDefaultExplorerUrls = ExplorerUrls "https://www.blockchain.com/btc-testnet" "https://www.blockchain.com/btc"

ergDefaultExplorerUrls :: ExplorerUrls
ergDefaultExplorerUrls = ExplorerUrls "https://testnet.ergoplatform.com" "https://explorer.ergoplatform.com"

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
  } deriving (Eq, Show, Read)

instance ToJSON BtcSettings where
  toJSON BtcSettings{..} = object [
      "explorerUrls" .= toJSON btcSettings'explorerUrls
    , "sendRbfByDefault" .= toJSON btcSettings'sendRbfByDefault
    ]

instance FromJSON BtcSettings where
  parseJSON = withObject "BtcSettings" $ \o -> do
    btcSettings'explorerUrls <- o .: "explorerUrls"
    btcSettings'sendRbfByDefault <- o .: "sendRbfByDefault"
    pure BtcSettings{..}

defaultBtcSettings :: BtcSettings
defaultBtcSettings = BtcSettings {
    btcSettings'explorerUrls = btcDefaultExplorerUrls
  , btcSettings'sendRbfByDefault = True
}

data ErgoSettings = ErgoSettings {
    ergSettings'explorerUrls :: !ExplorerUrls
} deriving (Eq, Show, Read)

instance ToJSON ErgoSettings where
  toJSON ErgoSettings{..} = object [
      "explorerUrls" .= toJSON ergSettings'explorerUrls
    ]

instance FromJSON ErgoSettings where
  parseJSON = withObject "ErgoSettings" $ \o -> do
    ergSettings'explorerUrls <- o .: "explorerUrls"
    pure ErgoSettings{..}

defaultErgSettings :: ErgoSettings
defaultErgSettings = ErgoSettings {
    ergSettings'explorerUrls = ergDefaultExplorerUrls
}

data CurrencySettings = SettingsBtc !BtcSettings | SettingsErgo !ErgoSettings
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''CurrencySettings)

type CurrencySpecificSettings = M.Map Currency CurrencySettings

defaultCurrencySpecificSettings :: CurrencySpecificSettings
defaultCurrencySpecificSettings = M.fromList $ btcDefaultSettings <> ergoDefaultSettings
  where
    btcDefaultSettings  = [(BTC, SettingsBtc defaultBtcSettings)]
    ergoDefaultSettings = [(ERGO, SettingsErgo defaultErgSettings)]

getBtcSettings :: Settings -> BtcSettings
getBtcSettings settings = case M.lookup BTC (settingsCurrencySpecific settings) of
  Just (SettingsBtc btcSettings) -> btcSettings
  _ -> defaultBtcSettings

getErgoSettings :: Settings -> ErgoSettings
getErgoSettings settings = case M.lookup ERGO (settingsCurrencySpecific settings) of
  Just (SettingsErgo ergoSettings) -> ergoSettings
  _ -> defaultErgSettings

data Settings = Settings {
  settingsLang              :: Language
, settingsStoreDir          :: Text
, settingsConfigPath        :: Text
, settingsUnits             :: Maybe Units
, settingsReqTimeout        :: NominalDiffTime
, settingsActiveAddrs       :: [Text]
, settingsDeactivatedAddrs  :: [Text]
, settingsArchivedAddrs     :: [Text]
, settingsReqUrlNum         :: (Int, Int) -- ^ First is minimum required answers. Second is sufficient amount of answers from indexers.
, settingsActUrlNum         :: Int
, settingsPortfolio         :: Bool
, settingsFiatCurr          :: Maybe Fiat
, settingsRateFiat          :: Maybe Fiat
, settingsDns               :: S.Set HostName
, settingsSocksProxy        :: Maybe SocksConf
, settingsCurrencySpecific  :: CurrencySpecificSettings
} deriving (Eq, Show)

makeLensesWith humbleFields ''Settings

$(deriveJSON defaultOptions ''PortNumber)
$(deriveJSON defaultOptions ''SockAddr)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    settingsLang              <- o .:  "lang"
    settingsStoreDir          <- o .:  "storeDir"
    settingsConfigPath        <- o .:  "configPath"
    settingsUnits             <- o .:  "units"
    settingsReqTimeout        <- o .:  "reqTimeout"
    settingsActiveAddrs       <- o .:  "activeAddrs"      .!= mempty
    settingsDeactivatedAddrs  <- o .:  "deactivatedAddrs" .!= mempty
    settingsArchivedAddrs     <- o .:  "archivedAddrs"    .!= mempty
    settingsReqUrlNum         <- o .:? "reqUrlNum"        .!= defaultIndexersNum
    settingsActUrlNum         <- o .:? "actUrlNum"        .!= 10
    settingsPortfolio         <- o .:? "portfolio"        .!= False
    settingsFiatCurr          <- o .:? "fiatCurr"
    settingsRateFiat          <- o .:? "rateFiat"
    mdns                      <- o .:? "dns"
    settingsSocksProxy        <- o .:? "socksProxy"
    let settingsDns = maybe defaultDns S.fromList mdns
    settingsCurrencySpecific  <- o .:? "currencySpecific" .!= defaultCurrencySpecificSettings
    pure Settings{..}

instance ToJSON Settings where
  toJSON Settings{..} = object [
      "lang"              .= toJSON settingsLang
    , "storeDir"          .= toJSON settingsStoreDir
    , "configPath"        .= toJSON settingsConfigPath
    , "units"             .= toJSON settingsUnits
    , "reqTimeout"        .= toJSON settingsReqTimeout
    , "activeAddrs"       .= toJSON settingsActiveAddrs
    , "deactivatedAddrs"  .= toJSON settingsDeactivatedAddrs
    , "archivedAddrs"     .= toJSON settingsArchivedAddrs
    , "reqUrlNum"         .= toJSON settingsReqUrlNum
    , "actUrlNum"         .= toJSON settingsActUrlNum
    , "portfolio"         .= toJSON settingsPortfolio
    , "fiatCurr"          .= toJSON settingsFiatCurr
    , "rateFiat"          .= toJSON settingsRateFiat
    , "dns"               .= toJSON settingsDns
    , "socksProxy"        .= toJSON settingsSocksProxy
    , "currencySpecific"  .= toJSON settingsCurrencySpecific
   ]

defIndexerPort :: PortNumber
defIndexerPort = defNodePort isTestnet

defaultIndexers :: [Text]
defaultIndexers = defNodes isTestnet

defaultSeedNodesSource :: [Domain]
defaultSeedNodesSource = defSeedNodesSource isTestnet

defaultIndexersNum :: (Int, Int)
defaultIndexersNum = (2, 4)

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultActUrlNum :: Int
defaultActUrlNum = 10

defaultDns :: S.Set HostName
defaultDns = S.fromList $ defDns isAndroid

defaultSettings :: FilePath -> Settings
defaultSettings home =
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
  in Settings {
        settingsLang              = English
      , settingsStoreDir          = pack storePath
      , settingsConfigPath        = pack configPath
      , settingsUnits             = Just defUnits
      , settingsReqTimeout        = defaultIndexerTimeout
      , settingsReqUrlNum         = defaultIndexersNum
      , settingsActUrlNum         = defaultActUrlNum
      , settingsPortfolio         = False
      , settingsFiatCurr          = Nothing
      , settingsRateFiat          = Nothing
      , settingsActiveAddrs       = []
      , settingsDeactivatedAddrs  = []
      , settingsArchivedAddrs     = []
      , settingsDns               = defaultDns
      , settingsSocksProxy        = Nothing
      , settingsCurrencySpecific  = defaultCurrencySpecificSettings
      }

-- | TODO: Implement some checks to see if the configPath folder is ok to write to
storeSettings :: MonadIO m => Settings -> m ()
storeSettings s = liftIO $ do
  let configPath = settingsConfigPath s
  createDirectoryIfMissing True $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd "/" configPath
  encodeFile (unpack configPath) s

#ifdef ANDROID
loadSettings :: (MonadIO m, PlatformNatives) => Maybe FilePath -> m Settings
loadSettings = const $ liftIO $ do
  mpath <- getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let configPath = path <> "/config.yaml"
      ex <- doesFileExist configPath
      cfg <- if not ex
        then pure $ defaultSettings path
        else fmap (either (const $ defaultSettings path) id) $ readYamlEither' configPath
      createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
      encodeFile (unpack $ settingsConfigPath cfg) cfg
      pure cfg

#else
mkDefSettings :: MonadIO m => m Settings
mkDefSettings = liftIO $ do
  home <- getHomeDirectory
  putStrLn   "[ WARNING ]: Failed to load config. Reverting to default values: "
  putStrLn $ "Config path: " <> home <> "/.ergvein/config.yaml"
  putStrLn $ "Store  path: " <> home <> "/.ergvein/store"
  putStrLn $ "Language   : English"
  pure $ defaultSettings (home <> "/.ergvein")

loadSettings :: MonadIO m => Maybe FilePath -> m Settings
loadSettings mpath = liftIO $ case mpath of
  Nothing -> do
    home <- getHomeDirectory
    let path = home <> "/.ergvein/config.yaml"
    putStrLn "[ WARNING ]: No path provided. Trying the default: "
    putStrLn path
    loadSettings $ Just path
  Just path -> do
    ex <- doesFileExist path
    cfg <- if not ex
      then mkDefSettings
      else either (const mkDefSettings) pure =<< readYamlEither' path
    createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
    encodeFile (unpack $ settingsConfigPath cfg) cfg
    pure cfg
#endif
