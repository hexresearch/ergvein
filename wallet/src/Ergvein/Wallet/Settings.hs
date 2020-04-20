{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , loadSettings
  , storeSettings
  , defaultSettings
  , defaultIndexers
  , defaultIndexersNum
  , defaultIndexerTimeout
  , defaultActUrlNum
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (encodeFile)
import Data.Maybe
import Data.Default
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Language
import Ergvein.Wallet.Native
import Ergvein.Wallet.Yaml(readYamlEither')
import qualified Data.Map.Strict as M
import Servant.Client(BaseUrl(..), parseBaseUrl)
import System.Directory

import qualified Data.Text as T

#ifdef ANDROID
import Android.HaskellActivity
#endif

data Settings = Settings {
  settingsLang              :: Language
, settingsStoreDir          :: Text
, settingsConfigPath        :: Text
, settingsUnits             :: Maybe Units
, settingsActiveCurrencies  :: ActiveCurrencies
, settingsReqTimeout        :: NominalDiffTime
, settingsActiveUrls        :: [BaseUrl]
, settingsDeactivatedUrls   :: [BaseUrl]
, settingsPassiveUrls       :: [BaseUrl]
, settingsReqUrlNum         :: (Int, Int) -- ^ First is minimum required answers. Second is sufficient amount of answers from indexers.
, settingsActUrlNum         :: Int
, settingsNodes             :: M.Map Currency [BaseUrl]
} deriving (Eq, Show)


makeLensesWith humbleFields ''Settings

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    settingsLang              <- o .: "lang"
    settingsStoreDir          <- o .: "storeDir"
    settingsConfigPath        <- o .: "configPath"
    settingsUnits             <- o .: "units"
    settingsActiveCurrencies  <- o .: "activeCurrencies"
    settingsReqTimeout        <- o .: "reqTimeout"
    mActiveUrls               <- o .: "activeUrls"
    mDeactivatedUrls          <- o .: "deactivatedUrls"
    mPassiveUrls              <- o .: "passiveUrls"
    settingsReqUrlNum         <- o .:? "reqUrlNum"  .!= defaultIndexersNum
    settingsActUrlNum         <- o .:? "actUrlNum"  .!= 10
    let (settingsActiveUrls, settingsDeactivatedUrls, settingsPassiveUrls) =
          case (mActiveUrls, mDeactivatedUrls, mPassiveUrls) of
            (Nothing, Nothing, Nothing) -> (defaultIndexers, [], [])
            (Just [], Just [], Just []) -> (defaultIndexers, [], [])
            _ -> (fromMaybe [] mActiveUrls, fromMaybe [] mDeactivatedUrls, fromMaybe [] mPassiveUrls)
    settingsNodes             <- o .:? "nodes" .!= defaultNodes
    pure Settings{..}

instance ToJSON Settings where
  toJSON Settings{..} = object [
      "lang"              .= toJSON settingsLang
    , "storeDir"          .= toJSON settingsStoreDir
    , "configPath"        .= toJSON settingsConfigPath
    , "units"             .= toJSON settingsUnits
    , "activeCurrencies"  .= toJSON settingsActiveCurrencies
    , "reqTimeout"        .= toJSON settingsReqTimeout
    , "activeUrls"        .= toJSON settingsActiveUrls
    , "deactivatedUrls"   .= toJSON settingsDeactivatedUrls
    , "passiveUrls"       .= toJSON settingsPassiveUrls
    , "reqUrlNum"         .= toJSON settingsReqUrlNum
    , "actUrlNum"         .= toJSON settingsActUrlNum
    , "nodes"             .= toJSON settingsNodes
   ]

defaultIndexers :: [BaseUrl]
defaultIndexers = [
    parse "https://ergvein-indexer1.hxr.team"
  , parse "https://ergvein-indexer2.hxr.team"
  ]
  where
    parse = either (error . ("Failed to parse default indexer: " ++) . show) id . parseBaseUrl

defaultNodes :: M.Map Currency [BaseUrl]
defaultNodes = M.fromList $ [
    (BTC, [
      parse "79.155.84.176:18333"
    ])
  , (ERGO, [
      parse "127.0.0.1"
    , parse "127.0.0.2"])]
  where
    parse = either (error . ("Failed to parse default indexer: " ++) . show) id . parseBaseUrl

defaultIndexersNum :: (Int, Int)
defaultIndexersNum = (2, 4)

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultActUrlNum :: Int
defaultActUrlNum = 10

defaultSettings :: FilePath -> Settings
defaultSettings home =
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
  in Settings {
        settingsLang              = English
      , settingsStoreDir          = pack storePath
      , settingsConfigPath        = pack configPath
      , settingsUnits             = Just defUnits
      , settingsActiveCurrencies  = ActiveCurrencies mempty
      , settingsReqTimeout        = defaultIndexerTimeout
      , settingsActiveUrls        = defaultIndexers
      , settingsDeactivatedUrls   = []
      , settingsPassiveUrls       = []
      , settingsReqUrlNum         = defaultIndexersNum
      , settingsActUrlNum         = defaultActUrlNum
      , settingsNodes             = defaultNodes
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
      logWrite $ "Loaded settings: " <> showt cfg
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
