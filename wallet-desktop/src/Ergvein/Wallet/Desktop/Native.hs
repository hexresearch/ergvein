module Ergvein.Wallet.Desktop.Native(

  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text(Text)
import Data.Time.LocalTime (getCurrentTimeZone)
import Ergvein.Aeson
import Ergvein.Wallet.Native
import Network.DNS.Resolver
import System.Directory
import System.Directory.Tree
import System.FilePath.Posix
import System.Hclip
import System.IO
import System.X509 (getSystemCertificateStore)
import Web.Browser

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Turtle

instance PlatformNatives where
  resUrl = id

  storeValue k v atomicMode = do
    path <- getStoreDir
    logWrite $ "Writing file " <> path <> "/" <> k
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> k
      createDirectoryIfMissing True $ takeDirectory fpath
      case atomicMode of
        True -> do
          tmpDir <- T.pack <$> getTemporaryDirectory
          let tmpFilePath = T.unpack $ tmpDir <> "/ergvein/" <> k
          createDirectoryIfMissing True $ takeDirectory tmpFilePath
          writeJsonAtomic tmpFilePath v
          Turtle.mv (Turtle.fromText $ T.pack tmpFilePath) (Turtle.fromText $ T.pack fpath)
        False -> writeJson fpath v

  retrieveValue k a0 = do
    path <- getStoreDir
    logWrite $ "Reading file " <> path <> "/" <> k
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> k
      ex <- doesFileExist fpath
      if ex
        then do
          key <- readJson fpath
          pure $ maybe (Left $ NADecodingError k) Right key
        else pure $ Right a0

  listKeys = do
    path <- getStoreDir
    liftIO $ fmap (T.drop (T.length path + 1) . T.pack) <$> getFiles (T.unpack path)

  readStoredFile filename = do
    path <- getStoreDir
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> filename
      ex <- doesFileExist fpath
      if ex
        then do
          cnt <- T.readFile fpath
          pure $ Right $ T.lines cnt
        else pure $ Left $ NAFileDoesNotExist filename

  appendStoredFile filename cnt = do
    path <- getStoreDir
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> filename
      ex <- doesFileExist fpath
      if ex
        then T.appendFile fpath cnt
        else T.writeFile fpath cnt

  moveStoredFile filename1 filename2 = do
    path <- getStoreDir
    logWrite $ "Moving file " <> path <> "/" <> filename1 <> " to " <> path <> "/" <> filename2
    liftIO $ do
      let fpath1 = T.unpack $ path <> "/" <> filename1
          fpath2 = T.unpack $ path <> "/" <> filename2
      ex <- doesFileExist fpath1
      if ex
        then Right <$> renameFile fpath1 fpath2
        else pure $ Left $ NAFileDoesNotExist filename1

  getStoreFileSize filename = do
    path <- getStoreDir
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> filename
      ex <- doesFileExist fpath
      if ex
        then fmap Right $ withFile fpath ReadMode $ fmap fromIntegral . hFileSize
        else pure $ Left $ NAFileDoesNotExist filename

  pasteStr = liftIO $ fmap T.pack getClipboard

  copyStr = liftIO . setClipboard . T.unpack

  getTimeZone = liftIO getCurrentTimeZone

  shareUrl = liftIO . setClipboard . T.unpack

  openUrl = liftIO . void . openBrowser . T.unpack

  -- TODO: Fix later for desktop application
  cameraWork = liftIO . setClipboard . T.unpack

  -- TODO: Fix later for desktop application
  cameraGetResult = liftIO $ pure "TempDesktop"

  logWrite = liftIO . T.putStrLn

  readSystemCertificates = liftIO getSystemCertificateStore

  nativeResolvConf = defaultResolvConf
  {-# INLINE nativeResolvConf #-}

  nativeShareJpeg _ _ = pure ()

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  _ :/ tree <- build dir
  pure $ reverse $ go tree
  where
    go tree = case tree of
      File _ n -> [n]
      Dir _ trees -> concat . fmap go $ trees
      _ -> []
