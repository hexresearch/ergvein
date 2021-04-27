{-# LANGUAGE TypeApplications #-}
module Ergvein.Filters.Btc.TestHelpers where

import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text, unpack)
import Ergvein.Text
import Network.Haskoin.Address
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Script
import Network.Haskoin.Transaction

import qualified Data.ByteString.Base16        as BS16
import qualified Data.Serialize                as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Ergvein.Filters.Btc           as IM
import qualified Ergvein.Filters.Btc.Mutable   as M


loadAddress :: Text -> Address
loadAddress t =
  fromMaybe (error "Failed to parse address")
    $ stringToAddr btcTest t

loadAddressMainnet :: Text -> Address
loadAddressMainnet t =
  fromMaybe (error "Failed to parse address")
    $ stringToAddr btc t

loadScript :: Text -> ByteString
loadScript = hex2bs

loadTx :: Text -> Tx
loadTx = either error id . S.decode @Tx . hex2bs

loadBlock :: Text -> Block
loadBlock = either error id . S.decode @Block . hex2bs . T.filter (/= '\n')

loadBlockHash :: Text -> BlockHash
loadBlockHash = fromMaybe (error "Failed to parse block hash") . hexToBlockHash

loadFilterMut :: Text -> IO M.BtcAddrFilter
loadFilterMut v = either (fail "Failed to decode filter!") pure =<< M.decodeBtcAddrFilter (hex2bs v)

loadFilter :: Text -> IM.BtcAddrFilter
loadFilter v = either (error "Failed to decode filter!") id $ IM.decodeBtcAddrFilter (hex2bs v)

showScript :: ScriptOp -> String
showScript (OP_PUSHDATA bs opt) = "OP_PUSHDATA " <> unpack (TE.decodeUtf8 (BS16.encode bs)) <> " " <> show opt
showScript op = show op
