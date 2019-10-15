module Ergvein.Wallet.Native(
    resUrl
  , storeValue
  , retrieveValue
  , readStoredFile
  , appendStoredFile
  , moveStoredFile
  , getStoreFileSize
  , pasteStr
  , copyStr
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text(Text)

resUrl :: Text -> Text
resUrl = undefined

storeValue :: (MonadIO m, ToJSON a) => Text -> a -> m (Either Text ())
storeValue = undefined

retrieveValue :: (MonadIO m, FromJSON a) => Text -> a -> m (Either Text a)
retrieveValue = undefined

-- | Read stored file line by line from android app folder. Non existing file
-- will return empty list.
readStoredFile :: (MonadIO m) => Text -> m [Text]
readStoredFile = undefined

-- | Write down a value to stored file at its end.
appendStoredFile :: (MonadIO m) => Text -> Text -> m ()
appendStoredFile = undefined

-- | Move stored file from first name to the second with destruction of second.
moveStoredFile :: (MonadIO m) => Text -> Text -> m ()
moveStoredFile = undefined

-- | Get size in bytes of stored file
getStoreFileSize :: MonadIO m => Text -> m Int
getStoreFileSize = undefined

-- | Get contents of clipboard
pasteStr :: MonadIO m => m Text
pasteStr = undefined

-- | Put string into clipboard
copyStr :: MonadIO m => Text -> m ()
copyStr = undefined
