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
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (encodeFile)
import Data.Maybe
import Data.Either
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Network.Socket
import System.Directory
import Data.IP
import Network.DNS.Lookup
import Network.DNS.Types
import Network.DNS.Resolver
import Ergvein.Wallet.Platform

import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Yaml(readYamlEither')
import Ergvein.Text

import qualified Data.Map.Strict as M
import qualified Data.Text as T

#ifdef ANDROID
import Android.HaskellActivity
import Ergvein.Wallet.Native
#endif

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
, settingsExplorerUrl       :: M.Map Currency ExplorerUrls
, settingsPortfolio         :: Bool
, settingsFiatCurr          :: Fiat
, settingsDns               :: [HostName]
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
    mActiveAddrs              <- o .: "activeAddrs"
    mDeactivatedAddrs         <- o .: "deactivatedAddrs"
    mArchivedAddrs            <- o .: "archivedAddrs"
    settingsReqUrlNum         <- o .:? "reqUrlNum"  .!= defaultIndexersNum
    settingsActUrlNum         <- o .:? "actUrlNum"  .!= 10
    let (settingsActiveAddrs, settingsDeactivatedAddrs, settingsArchivedAddrs) =
          case (mActiveAddrs, mDeactivatedAddrs, mArchivedAddrs) of
            (Nothing, Nothing, Nothing) -> ([], [], [])
            (Just [], Just [], Just []) -> ([], [], [])
            _ -> (fromMaybe [] mActiveAddrs, fromMaybe [] mDeactivatedAddrs, fromMaybe [] mArchivedAddrs)
    settingsExplorerUrl       <- o .:? "explorerUrl" .!= defaultExplorerUrl
    settingsPortfolio         <- o .:? "portfolio" .!= False
    settingsFiatCurr          <- o .:? "fiatCurr"  .!= USD
    mdns                      <- o .:? "dns"
    let settingsDns = case fromMaybe [] mdns of
          [] -> defaultDns
          dns -> dns
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
    , "explorerUrl"       .= toJSON settingsExplorerUrl
    , "portfolio"         .= toJSON settingsPortfolio
    , "fiatCurr"          .= toJSON settingsFiatCurr
    , "dns"               .= toJSON settingsDns
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

defaultDns :: [HostName]
defaultDns = ["8.8.8.8","8.8.4.4", "1.1.1.1"]

defaultIndexersNum :: (Int, Int)
defaultIndexersNum = (2, 4)

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultActUrlNum :: Int
defaultActUrlNum = 10

defaultSettings :: MonadIO m => FilePath -> m Settings
defaultSettings home = do
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"

  pure $ Settings {
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
      , settingsActiveAddrs       = []
      , settingsDeactivatedAddrs  = []
      , settingsArchivedAddrs     = []
      , settingsDns               = defaultDns
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
  defaultSettings (home <> "/.ergvein")

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