{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , loadSettings
  , storeSettings
  , defaultSettings
  , defIndexerPort
  , defaultIndexersNum
  , defaultIndexerTimeout
  , defaultActUrlNum
  , ExplorerUrls(..)
  , defaultExplorerUrl
  , btcDefaultExplorerUrls
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
  , PeerInfo (..)
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (encodeFile)
import Data.Maybe
import Data.Either
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Network.Socket (HostName, PortNumber)
import System.Directory
import Data.IP
import Network.DNS.Lookup
import Network.DNS.Types
import Network.DNS.Resolver
import Ergvein.Wallet.Platform
import Data.IP (IP, toSockAddr)
import Text.Read (readMaybe)
import Data.Word

import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.IP
import Ergvein.Wallet.Language
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Yaml(readYamlEither')
import Ergvein.Text

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Socks5 as S5
import qualified Data.Set as S

#ifdef ANDROID
import Android.HaskellActivity
import Ergvein.Wallet.Native
#endif



data PeerInfo = PeerInfo
  { peerInfoIsActive :: !Bool
  , peerInfoIsPinned   :: !Bool
  } deriving (Eq, Show)

instance ToJSON PeerInfo where
  toJSON PeerInfo{..} = object [
      "isActive" .= toJSON peerInfoIsActive
    , "isPinned" .= toJSON peerInfoIsPinned
   ]

instance FromJSON PeerInfo where
  parseJSON = withObject "v" $ \o -> do
    peerInfoIsActive <- o .: "isActive"
    peerInfoIsPinned <- o .: "isPinned"
    pure PeerInfo{..}

data ExplorerUrls = ExplorerUrls {
  testnetUrl :: !Text
, mainnetUrl :: !Text
} deriving (Eq, Show)

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

defaultExplorerUrl :: M.Map Currency ExplorerUrls
defaultExplorerUrl = M.fromList $ btcDefaultUrls <> ergoDefaultUrls
  where
    btcDefaultUrls  = [(BTC, btcDefaultExplorerUrls)]
    ergoDefaultUrls = [(ERGO, ExplorerUrls "" "")]

btcDefaultExplorerUrls :: ExplorerUrls
btcDefaultExplorerUrls = ExplorerUrls "https://www.blockchain.com/btc-testnet" "https://www.blockchain.com/btc"

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

data Settings = Settings {
  settingsLang              :: Language
, settingsStoreDir          :: Text
, settingsConfigPath        :: Text
, settingsUnits             :: Maybe Units
, settingsReqTimeout        :: NominalDiffTime
, settingsAddrs             :: M.Map Text PeerInfo
, settingsReqUrlNum         :: (Int, Int) -- ^ First is minimum required answers. Second is sufficient amount of answers from indexers.
, settingsActUrlNum         :: Int
, settingsExplorerUrl       :: M.Map Currency ExplorerUrls
, settingsPortfolio         :: Bool
, settingsDiscoveryEnabled  :: Bool
, settingsFiatCurr          :: Fiat
, settingsDns               :: S.Set HostName
, settingsSocksProxy        :: Maybe SocksConf
} deriving (Eq, Show)


makeLensesWith humbleFields ''Settings

$(deriveJSON defaultOptions ''PortNumber)
$(deriveJSON defaultOptions ''SockAddr)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    settingsLang              <- o .: "lang"
    settingsStoreDir          <- o .: "storeDir"
    settingsConfigPath        <- o .: "configPath"
    settingsUnits             <- o .: "units"
    settingsReqTimeout        <- o .: "reqTimeout"
    settingsReqUrlNum         <- o .:? "reqUrlNum"  .!= defaultIndexersNum
    settingsActUrlNum         <- o .:? "actUrlNum"  .!= 10
    settingsAddrs             <- o .:? "addrs"  .!= mempty
    settingsExplorerUrl       <- o .:? "explorerUrl" .!= defaultExplorerUrl
    settingsDiscoveryEnabled  <- o .:? "discoveryEnabled" .!= True
    settingsPortfolio         <- o .:? "portfolio" .!= False
    settingsFiatCurr          <- o .:? "fiatCurr"  .!= USD
    mdns                      <- o .:? "dns"
    settingsSocksProxy        <- o .:? "socksProxy"
    let settingsDns = case fromMaybe [] mdns of
          [] -> defaultDns
          dns -> S.fromList dns
    pure Settings{..}

instance ToJSON Settings where
  toJSON Settings{..} = object [
      "lang"              .= toJSON settingsLang
    , "storeDir"          .= toJSON settingsStoreDir
    , "configPath"        .= toJSON settingsConfigPath
    , "units"             .= toJSON settingsUnits
    , "reqTimeout"        .= toJSON settingsReqTimeout
    , "addrs"             .= toJSON settingsAddrs
    , "reqUrlNum"         .= toJSON settingsReqUrlNum
    , "actUrlNum"         .= toJSON settingsActUrlNum
    , "explorerUrl"       .= toJSON settingsExplorerUrl
    , "portfolio"         .= toJSON settingsPortfolio
    , "fiatCurr"          .= toJSON settingsFiatCurr
    , "dns"               .= toJSON settingsDns
    , "socksProxy"        .= toJSON settingsSocksProxy
   ]

defIndexerPort :: PortNumber
defIndexerPort = 8667

seedList :: [Domain]
seedList = if isTestnet
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
        settingsLang              = English
      , settingsStoreDir          = pack storePath
      , settingsConfigPath        = pack configPath
      , settingsUnits             = Just defUnits
      , settingsReqTimeout        = defaultIndexerTimeout
      , settingsReqUrlNum         = defaultIndexersNum
      , settingsActUrlNum         = defaultActUrlNum
      , settingsExplorerUrl       = defaultExplorerUrl
      , settingsPortfolio         = False
      , settingsFiatCurr          = USD
      , settingsAddrs             = M.singleton "127.0.0.1:8667" $ PeerInfo  True True
      , settingsDiscoveryEnabled  = True
      , settingsDns               = defaultDns
      , settingsSocksProxy        = Nothing
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
        then defaultSettings path
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