module Ergvein.Wallet.Android.Native(

  ) where

import Android.HaskellActivity
import Control.Exception (handle, bracket, SomeException)
import Control.Monad.IO.Class
import Data.Text(Text, unpack)
import Ergvein.Aeson
import Ergvein.Wallet.Native
import Foreign
import Foreign.C
import System.Directory
import System.Directory.Tree
import System.FilePath.Posix
import System.IO

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

newtype JString = JString { unJString :: Ptr JString }

foreign import ccall safe "android_paste_str" androidPasteStr :: HaskellActivity -> IO JString
foreign import ccall safe "read_paste_str" androidReadPasteStr :: JString -> IO CString
foreign import ccall safe "release_paste_str" androidReleasePasteStr :: JString -> CString -> IO ()
foreign import ccall safe "android_copy_str" androidCopyStr :: HaskellActivity -> CString -> IO ()
foreign import ccall safe "android_log_write" androidLogWrite :: CString -> IO ()

foreign import ccall safe "android_timezone_offset" androidTimezoneOffset :: IO Int

foreign import ccall safe "android_share_url" androidShareUrl :: HaskellActivity -> CString -> IO ()

decodeText :: CString -> IO Text
decodeText cstr = do
  bytestr <- BS.packCString cstr
  return (T.decodeUtf8 bytestr)

encodeText :: Text -> (CString -> IO a) -> IO a
encodeText text cont =
  BS.useAsCString (T.encodeUtf8 text) cont

instance PlatformNatives where
  resUrl = (<>) "file:///android_res/"

  storeValue k v = do
    path <- getStoreDir
    logWrite $ "Writing file " <> path <> "/" <> k
    liftIO $ do
      let fpath = T.unpack $ path <> "/" <> k
      createDirectoryIfMissing True $ takeDirectory fpath
      writeJson fpath v

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
    logWrite $ "Reading file " <> path <> "/" <> filename
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
    logWrite $ "Appending file " <> path <> "/" <> filename
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

  pasteStr = liftIO $ do
    jstring <- androidPasteStr =<< getHaskellActivity
    bracket allocPaste releasePaste $ decodeText . snd
    where
      allocPaste = do
        a <- getHaskellActivity
        jstring <- androidPasteStr a
        cstr <- androidReadPasteStr jstring
        pure (jstring, cstr)
      releasePaste (jstring, cstr) = androidReleasePasteStr jstring cstr

  copyStr v = liftIO $ encodeText v $ \s -> do
    a <- getHaskellActivity
    androidCopyStr a s

  shareUrl v = liftIO $ encodeText v $ \s -> do
    a <- getHaskellActivity
    androidShareUrl a s

  logWrite v = liftIO $ encodeText v androidLogWrite
  {-# INLINE logWrite #-}

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  _ :/ tree <- build dir
  pure $ reverse $ go tree
  where
    go tree = case tree of
      File _ n -> [n]
      Dir _ trees -> concat . fmap go $ trees
      _ -> []
