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
  , defIndexerPort
  , defaultIndexersNum
  , defaultIndexerTimeout
  , defaultActUrlNum
  , ExplorerUrls(..)
  , defaultDns
  , defaultIndexers
  , getDNS
  , seedList
  , SocksConf(..)
  , torSocks
  , toSocksProxy
  -- * Helpers
  , makeSockAddr
  , parseIP
  , ErgveinNodeManagementInfo (..)
  , settingsLang           
  , settingsStoreDir         
  , settingsConfigPath       
  , settingsUnits            
  , settingsReqTimeout       
  , settingsErgveinNetwork            
  , settingsReqUrlNum        
  , settingsActUrlNum        
  , settingsPortfolio        
  , settingsDiscoveryEnabled 
  , settingsFiatCurr         
  , settingsDns              
  , settingsSocksProxy       
  , settingsCurrencySpecific
  , nfoIsActivated
  , nfoIsUserModified
  , ErgveinNodeAddr
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (encodeFile)
import Data.Either
import Data.IP
import Data.IP (IP, toSockAddr)
import Data.Maybe
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Word
import Data.Yaml (encodeFile)
import Ergvein.Wallet.Platform
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.DNS.Types
import Network.Socket (HostName, PortNumber)
import System.Directory

import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.IP
import Ergvein.Wallet.Language
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Yaml(readYamlEither')

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Network.Socks5  as S5

#ifdef ANDROID
import Android.HaskellActivity
import Ergvein.Wallet.Native
#endif

type ErgveinNodeAddr = Text

data ErgveinNodeManagementInfo = ErgveinNodeManagementInfo
  { _nfoIsActivated  :: !Bool
  , _nfoIsUserModified :: !Bool
  } deriving (Eq, Show)

makeLenses ''ErgveinNodeManagementInfo

instance ToJSON ErgveinNodeManagementInfo where
  toJSON ErgveinNodeManagementInfo{..} = object [
      "nfoIsActivated" .= toJSON _nfoIsActivated
    , "nfoIsUserModified" .= toJSON _nfoIsUserModified
   ]

instance FromJSON ErgveinNodeManagementInfo where
  parseJSON = withObject "v" $ \o -> do
    _nfoIsActivated    <- o .: "nfoIsActivated"
    _nfoIsUserModified <- o .: "nfoIsUserModified"
    pure ErgveinNodeManagementInfo{..}

data ExplorerUrls = ExplorerUrls {
  testnetUrl :: !Text
, mainnetUrl :: !Text
} deriving (Eq, Show, Read)

instance ToJSON ExplorerUrls where
  toJSON ExplorerUrls{..} = object [ "testnetUrl"  .= toJSON testnetUrl
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
getBtcSettings settings = case M.lookup BTC (_settingsCurrencySpecific settings) of
  Just (SettingsBtc btcSettings) -> btcSettings
  _ -> defaultBtcSettings

getErgoSettings :: Settings -> ErgoSettings
getErgoSettings settings = case M.lookup ERGO (_settingsCurrencySpecific settings) of
  Just (SettingsErgo ergoSettings) -> ergoSettings
  _ -> defaultErgSettings

data Settings = Settings {
  _settingsLang              :: Language
, _settingsStoreDir          :: Text
, _settingsConfigPath        :: Text
, _settingsUnits             :: Maybe Units
, _settingsReqTimeout        :: NominalDiffTime
, _settingsErgveinNetwork    :: M.Map ErgveinNodeAddr ErgveinNodeManagementInfo
, _settingsReqUrlNum         :: (Int, Int) -- ^ First is minimum required answers. Second is sufficient amount of answers from indexers.
, _settingsActUrlNum         :: Int
, _settingsPortfolio         :: Bool
, _settingsDiscoveryEnabled  :: Bool
, _settingsFiatCurr          :: Fiat
, _settingsDns               :: S.Set HostName
, _settingsSocksProxy        :: Maybe SocksConf
, _settingsCurrencySpecific    :: CurrencySpecificSettings
} deriving (Eq, Show)

makeLenses ''Settings

$(deriveJSON defaultOptions ''PortNumber)
$(deriveJSON defaultOptions ''SockAddr)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    _settingsLang              <- o .:  "lang"
    _settingsStoreDir          <- o .:  "storeDir"
    _settingsConfigPath        <- o .:  "configPath"
    _settingsUnits             <- o .:  "units"
    _settingsReqTimeout        <- o .:  "reqTimeout"
    _settingsReqUrlNum         <- o .:? "reqUrlNum"        .!= defaultIndexersNum
    _settingsActUrlNum         <- o .:? "actUrlNum"        .!= 10
    _settingsErgveinNetwork    <- o .:? "ergveinNetwork"   .!= mempty
    _settingsDiscoveryEnabled  <- o .:? "discoveryEnabled" .!= True
    _settingsPortfolio         <- o .:? "portfolio"        .!= False
    _settingsFiatCurr          <- o .:? "fiatCurr"         .!= USD
    _mdns                      <- o .:? "dns"
    _settingsSocksProxy        <- o .:? "socksProxy"
    let _settingsDns = case fromMaybe [] _mdns of
          [] -> defaultDns
          dns -> S.fromList dns
    _settingsCurrencySpecific  <- o .:? "currencySpecific" .!= defaultCurrencySpecificSettings
    pure Settings{..}

instance ToJSON Settings where
  toJSON Settings{..} = object [
      "lang"              .= toJSON _settingsLang
    , "storeDir"          .= toJSON _settingsStoreDir
    , "configPath"        .= toJSON _settingsConfigPath
    , "units"             .= toJSON _settingsUnits
    , "reqTimeout"        .= toJSON _settingsReqTimeout
    , "ergveinNetwork"    .= toJSON _settingsErgveinNetwork
    , "reqUrlNum"         .= toJSON _settingsReqUrlNum
    , "actUrlNum"         .= toJSON _settingsActUrlNum
    , "portfolio"         .= toJSON _settingsPortfolio
    , "fiatCurr"          .= toJSON _settingsFiatCurr
    , "dns"               .= toJSON _settingsDns
    , "socksProxy"        .= toJSON _settingsSocksProxy
    , "discoveryEnabled"  .= toJSON _settingsDiscoveryEnabled
    , "currencySpecific"  .= toJSON _settingsCurrencySpecific
   ]

defIndexerPort :: PortNumber
defIndexerPort = 8667

seedList :: [Domain]
seedList = if False
  then ["testseed.cypra.io"]
  else ["seed.cypra.io"]

defaultIndexers :: [Text]
defaultIndexers = 
  if isTestnet 
  then ["127.0.0.1:8667"]
  else ["139.59.142.25:8667", "188.244.4.78:8667"]

defaultIndexersNum :: (Int, Int)
defaultIndexersNum = (2, 4)

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultActUrlNum :: Int
defaultActUrlNum = 10

defaultDns :: S.Set HostName
defaultDns = S.fromList $ if isAndroid
  then ["8.8.8.8","8.8.4.4", "1.1.1.1"]
  else [] -- use resolv.conf

defaultSettings :: FilePath -> Settings
defaultSettings home =
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
  in Settings {
        _settingsLang              = English
      , _settingsStoreDir          = pack storePath
      , _settingsConfigPath        = pack configPath
      , _settingsUnits             = Just defUnits
      , _settingsReqTimeout        = defaultIndexerTimeout
      , _settingsReqUrlNum         = defaultIndexersNum
      , _settingsActUrlNum         = defaultActUrlNum
      , _settingsPortfolio         = False
      , _settingsFiatCurr          = USD
      , _settingsErgveinNetwork    = mempty
      , _settingsDiscoveryEnabled  = True
      , _settingsDns               = defaultDns
      , _settingsSocksProxy        = Nothing
      , _settingsCurrencySpecific  = defaultCurrencySpecificSettings
      }

-- | TODO: Implement some checks to see if the configPath folder is ok to write to
storeSettings :: MonadIO m => Settings -> m ()
storeSettings s = liftIO $ do
  let configPath = _settingsConfigPath s
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
        then defaultSettings path
        else fmap (either (const $ defaultSettings path) id) $ readYamlEither' configPath
      createDirectoryIfMissing True (unpack $ settingsStoreDir ^. cfg)
      encodeFile (unpack $ _settingsConfigPath cfg) cfg
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
    createDirectoryIfMissing True (unpack $ cfg ^. settingsStoreDir)
    encodeFile (unpack $ _settingsConfigPath cfg) cfg
    pure cfg
#endif

getDNS :: ResolvSeed -> [Domain] -> IO (Maybe [Text])
getDNS seed domains = withResolver seed $ \resolver -> do 
  findMapMMaybe (resolve resolver) domains
  where
    resolve :: Resolver -> Domain -> IO (Maybe [Text])
    resolve resolver domain = do
        v4 <- lookupA resolver domain
        v6 <- lookupAAAA resolver domain
        let resolved = concat $ rights [(fmap showt <$> v4), (fmap showt <$> v6)]
        pure $ if length resolved < 2 then Nothing else Just resolved
    
    findMapMMaybe :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
    findMapMMaybe f (x:xs) = do
      r <- f x
      if isJust r then
        pure r
      else
        findMapMMaybe f xs
    findMapMMaybe f [] = pure Nothing