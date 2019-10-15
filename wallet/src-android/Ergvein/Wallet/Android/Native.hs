module Ergvein.Wallet.Native.Android(

  ) where

import Android.HaskellActivity
import Control.Exception (handle, bracket, SomeException)
import Control.Monad.IO.Class
import Data.Text(Text, unpack)
import Foreign
import Foreign.C
import Ergvein.Aeson
import System.Directory
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

foreign import ccall safe "android_timezone_offset" androidTimezoneOffset :: IO Int

decodeText :: CString -> IO Text
decodeText cstr = do
  bytestr <- BS.packCString cstr
  return (T.decodeUtf8 bytestr)

encodeText :: Text -> (CString -> IO a) -> IO a
encodeText text cont =
  BS.useAsCString (T.encodeUtf8 text) cont

instance PlatformNatives where
  storeValue k v = liftIO $ do
    mpath <- getFilesDir =<< getHaskellActivity
    case mpath of
      Nothing -> pure $ "No such path " <> k
      Just path -> let fpath = path <> "/" <> unpack k in do
        createDirectoryIfMissing True $ takeDirectory fpath
        writeJson fpath v
        pure $ Right ()

  retrieveValue k a0 = liftIO $ do
    mpath <- getFilesDir =<< getHaskellActivity
    case mpath of
      Nothing -> pure $ "No such path " <> k
      Just path -> let fpath = path <> "/" <> unpack k in do
        ex <- doesFileExist fpath
        if ex
          then do
            key <- readJson fpath
            pure $ maybe (Left $ "Decoding error for key " <> k) Right key
          else pure $ Right a0

  readStoredFile filename = liftIO $ do
    mpath <- getFilesDir =<< getHaskellActivity
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
    mpath <- getFilesDir =<< getHaskellActivity
    case mpath of
      Nothing -> pure ()
      Just path -> let fpath = path <> "/" <> unpack filename in do
        ex <- doesFileExist fpath
        if ex
          then T.appendFile fpath cnt
          else T.writeFile fpath cnt

  moveStoredFile filename1 filename2 = liftIO $ do
    mpath <- getFilesDir =<< getHaskellActivity
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
    mpath <- getFilesDir =<< getHaskellActivity
    case mpath of
      Nothing -> pure 0
      Just path -> let fpath = path <> "/" <> unpack filename in do
        ex <- doesFileExist fpath
        if ex
          then withFile fpath ReadMode $ fmap fromIntegral . hFileSize
          else pure 0


  resUrl = (<>) "file:///android_res/"

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
