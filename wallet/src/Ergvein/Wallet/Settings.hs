{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , loadSettings
  , storeSettings
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson (withText)
import Data.Default
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Text
import Ergvein.Types.Currency (Units(..), defUnits, Currency, allCurrencies)
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Language
import Ergvein.Wallet.Native
import Ergvein.Wallet.Yaml(readYamlEither')
import qualified Data.Map.Strict as M
import Servant.Client(BaseUrl(..), parseBaseUrl)
import System.Directory

import qualified Data.Text as T
import qualified Data.Map as Map

#ifdef ANDROID
import Android.HaskellActivity
#endif

data Settings = Settings {
  settingsLang              :: Language
, settingsStoreDir          :: Text
, settingsConfigPath        :: Text
, settingsDefUrls           :: [BaseUrl]
, settingsDefUrlNum         :: (Int, Int) -- ^ First is minimum required answers. Second is sufficient amount of answers from indexers.
, settingsReqTimeout        :: NominalDiffTime
, settingsUnits             :: Maybe Units
, settingsActiveCurrencies  :: ActiveCurrencies
} deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "settings") ''Settings)

makeLensesWith humbleFields ''Settings

-- | TODO: Implement some checks to see if the configPath folder is ok to write to
storeSettings :: MonadIO m => Settings -> m ()
storeSettings s = liftIO $ do
  let configPath = settingsConfigPath s
  createDirectoryIfMissing True $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd "/" configPath
  encodeFile (unpack configPath) s

defaultIndexers :: [BaseUrl]
defaultIndexers = [
    parse "https://ergvein-indexer1.hxr.team"
  , parse "https://ergvein-indexer2.hxr.team"
  , parse "https://ergvein-indexer3.hxr.team"
  , parse "https://ergvein-indexer4.hxr.team"
  ]
  where 
    parse = either (error . ("Failed to parse default indexer: " ++) . show) id . parseBaseUrl 

defaultIndexersNum :: (Int, Int)
defaultIndexersNum = (2, 4)

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultSettings :: FilePath -> Settings 
defaultSettings home =  
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
  in Settings {
        settingsLang = English
      , settingsStoreDir = pack storePath
      , settingsConfigPath = pack configPath
      , settingsDefUrls = defaultIndexers
      , settingsDefUrlNum = defaultIndexersNum
      , settingsReqTimeout = defaultIndexerTimeout
      , settingsUnits = Just defUnits
      , settingsActiveCurrencies = ActiveCurrencies mempty
      }

#ifdef ANDROID 
mkDefSettings :: MonadIO m => FilePath -> m Settings
mkDefSettings home = liftIO $ do
  let cfg = defaultSettings home
  createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
  encodeFile (unpack $ settingsConfigPath cfg) cfg
  pure cfg

loadSettings :: (MonadIO m, PlatformNatives) => Maybe FilePath -> m Settings
loadSettings = const $ liftIO $ do
  mpath <- getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let configPath = path <> "/config.yaml"
      ex <- doesFileExist configPath
      sett <- if not ex
        then mkDefSettings path
        else either (const $ mkDefSettings path) pure =<< readYamlEither' configPath
      logWrite $ "Loaded settings: " <> showt sett 
      pure sett 

#else
mkDefSettings :: MonadIO m => m Settings
mkDefSettings = liftIO $ do
  home <- getHomeDirectory
  putStrLn   "[ WARNING ]: Failed to load config. Reverting to default values: "
  putStrLn $ "Config path: " <> home <> "/.ergvein/config.yaml"
  putStrLn $ "Store  path: " <> home <> "/.ergvein/store"
  putStrLn $ "Language   : English"
  let cfg = defaultSettings (home <> "/.ergvein") 
  createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
  encodeFile (unpack $ settingsConfigPath cfg) cfg
  pure cfg

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
    if not ex
      then mkDefSettings
      else either (const mkDefSettings) pure =<< readYamlEither' path
#endif
