module Ergvein.Wallet.Desktop.Native(

  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text(Text)
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
  resUrl = id

  storeValue k v = do
    path <- getStoreDir
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> k
      createDirectoryIfMissing True $ takeDirectory fpath
      writeJson fpath v

  retrieveValue k a0 = do
    path <- getStoreDir
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> k
      ex <- doesFileExist fpath
      if ex
        then do
          key <- readJson fpath
          pure $ maybe (Left $ NADecodingError k) Right key
        else pure $ Right a0

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
    liftIO $ do
      let fpath1 = T.unpack $ path <> "/" <> filename1
          fpath2 = T.unpack $ path <> "/" <> filename2
      ex <- doesFileExist fpath1
      if ex
        then do
          T.writeFile fpath2 =<< T.readFile fpath1
          T.writeFile fpath1 ""
          pure $ Right ()
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
