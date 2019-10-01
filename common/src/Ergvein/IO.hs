module Ergvein.IO(
    readFileSafe
  , readLazyByteStringSafe
  , readStrictByteStringSafe
) where

import Data.Text (Text)
import System.Directory
import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T

readFileSafe :: FilePath -> IO (Maybe Text)
readFileSafe file = do
  isOk <- doesFileExist file
  if isOk
    then fmap Just $ T.readFile file
    else return Nothing

readLazyByteStringSafe :: FilePath -> IO (Maybe LB.ByteString)
readLazyByteStringSafe file = do
  catchIOError (fmap Just $ LB.readFile file) (const $ return Nothing)

readStrictByteStringSafe :: FilePath -> IO (Maybe BS.ByteString)
readStrictByteStringSafe file =
  catchIOError (fmap Just $ BS.readFile file) (const $ return Nothing)

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = Control.Exception.catch
