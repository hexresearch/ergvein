{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Index.Server.DB.Instances
  (
  --   getTxRec
  -- , getTxRecs
  -- , putTxInfosAsRecs
  -- , serializeToTxRec
  -- , serializeTxRec
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import Conversion
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Default
import Data.Flat
import Data.Foldable
import Data.Maybe
import Data.Time.Clock
import Database.LevelDB
import Database.LevelDB.Iterator
import Ergvein.Index.Server.Dependencies
import Servant.Client.Core

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming
import Ergvein.Index.Server.DB.Schema.Filters

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as Parse
import Data.ByteString.Builder

getTxRec :: (MonadIO m, MonadLogger m) => T.Text -> DB -> BS.ByteString -> m TxRec
getTxRec !caller db !key = do
  maybeResult <- get db def key
  case maybeResult of
    Just result -> either (const prntErr) pure $ deserializeTxRec result
    Nothing -> prntErr
  where
    prntErr = do
      currentTime <- liftIO getCurrentTime
      logErrorN $ "[Db read miss][getTxRec]" <> "["<> caller <>"]"<> " Entity with key " <> (T.pack $ show key) <> " not found at time:" <> (T.pack $ show currentTime)
      error $ "getTxRec: not found" ++ show key

getTxRecs :: (MonadIO m, MonadLogger m) => T.Text -> DB -> [BS.ByteString] -> m [TxRec]
getTxRecs !caller db !keys = traverse (getTxRec caller db) keys

deserializeTxRec :: BS.ByteString -> Either String TxRec
deserializeTxRec bs = flip parseOnly bs $ do
  txHashLen <- fromIntegral <$> anyWord32le
  txRecHash <- fmap (TxHash . BSS.toShort) $ Parse.take txHashLen
  txBytesLen <- fromIntegral <$> anyWord64le
  txRecBytes <- Parse.take txBytesLen
  txRecUnspentOutputsCount <- anyWord32le
  pure $ TxRec{..}

putTxInfosAsRecs :: [TxInfo] -> LDB.WriteBatch
putTxInfosAsRecs items = putI <$> items
  where putI item = LDB.Put (txRecKey $ txHash $ item) $ serializeToTxRec item

serializeToTxRec :: TxInfo -> BS.ByteString
serializeToTxRec TxInfo{..} = BL.toStrict . toLazyByteString $
      word32LE txHashLen
  <> byteString txh
  <> word64LE txBytesLen
  <> byteString txBytes
  <> word32LE txOutputsCount
  where
    txh = BSS.fromShort $ getTxHash txHash
    txHashLen = fromIntegral $ BS.length txh
    txBytesLen = fromIntegral $ BS.length txBytes

serializeTxRec :: TxRec -> BS.ByteString
serializeTxRec TxRec{..} = BL.toStrict . toLazyByteString $
      word32LE txHashLen
  <> byteString txh
  <> word64LE txBytesLen
  <> byteString txRecBytes
  <> word32LE txRecUnspentOutputsCount
  where
    txh = BSS.fromShort $ getTxHash txRecHash
    txHashLen = fromIntegral $ BS.length txh
    txBytesLen = fromIntegral $ BS.length txRecBytes
