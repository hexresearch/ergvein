module Ergvein.Wallet.Desktop.Native(

  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text(Text, pack, unpack)
import Ergvein.Aeson
import Ergvein.Wallet.Native
import System.Directory
import System.FilePath.Posix
import System.Hclip
import System.IO

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

instance PlatformNatives where
  getStoreDir = pure (Just ".ergvein")

  resUrl = id

  storeValue k v = liftIO $ do
    mpath <- getStoreDir
    case mpath of
      Nothing -> pure $ Left "Something is horribly wrong"
      Just path -> let fpath = path <> "/" <> unpack k in do
        createDirectoryIfMissing True $ takeDirectory fpath
        writeJson fpath v
        pure $ Right ()

  retrieveValue k a0 = liftIO $ do
    mpath <- getStoreDir
    case mpath of
      Nothing -> pure $ Left "Something is horribly wrong"
      Just path -> let fpath = path <> "/" <> unpack k in do
        ex <- doesFileExist fpath
        if ex
          then do
            key <- readJson fpath
            pure $ maybe (Left $ "Decoding error for key " <> k) Right key
          else pure $ Right a0

  readStoredFile filename = liftIO $ do
    mpath <- getStoreDir
    case mpath of
      Nothing -> pure []
      Just path -> let fpath = path <> "/" <> unpack filename in do
        ex <- doesFileExist fpath
        if ex
          then do
            cnt <- T.readFile fpath
            pure $ T.lines cnt
          else pure []

  appendStoredFile filename cnt = liftIO $ do
    mpath <- getStoreDir
    case mpath of
      Nothing -> pure ()
      Just path -> let fpath = path <> "/" <> unpack filename in do
        ex <- doesFileExist fpath
        if ex
          then T.appendFile fpath cnt
          else T.writeFile fpath cnt

  moveStoredFile filename1 filename2 = liftIO $ do
    mpath <- getStoreDir
    case mpath of
      Nothing -> pure ()
      Just path -> do
        let fpath1 = path <> "/" <> unpack filename1
            fpath2 = path <> "/" <> unpack filename2
        ex <- doesFileExist fpath1
        if ex
          then do
            T.writeFile fpath2 =<< T.readFile fpath1
            T.writeFile fpath1 ""
          else pure ()

  getStoreFileSize filename = liftIO $ do
    mpath <- getStoreDir
    case mpath of
      Nothing -> pure 0
      Just path -> let fpath = path <> "/" <> unpack filename in do
        ex <- doesFileExist fpath
        if ex
          then withFile fpath ReadMode $ fmap fromIntegral . hFileSize
          else pure 0

  pasteStr = liftIO $ fmap T.pack getClipboard

  copyStr = liftIO . setClipboard . T.unpack
