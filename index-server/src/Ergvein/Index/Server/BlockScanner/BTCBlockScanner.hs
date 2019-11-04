module Ergvein.Index.Server.BlockScanner.BTCBlockScanner where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad
import Data.Maybe
import Data.Either
import Database.Persist.Sql
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Client

import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction


import qualified Data.ByteString                as B
import           Data.Serialize                 as S
import qualified Data.HexString as HS
import qualified Network.Haskoin.Block as HK
import qualified Network.Haskoin.Transaction as HK
import qualified Network.Haskoin.Crypto as HK
import           Network.Haskoin.Util
import qualified Data.Text.IO as T
import Data.Text (Text, pack, unpack)
import Data.List.Index

btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient cfg = withClient 
  (configBTCNodeHost     cfg)
  (configBTCNodePort     cfg)
  (configBTCNodeUser     cfg)
  (configBTCNodePassword cfg)

btcBlock :: HS.HexString -> HK.Block
btcBlock = (fromRight $ error "error parsing block") . decode . HS.toBytes

scriptOutputHash :: B.ByteString -> PubKeyScriptHash
scriptOutputHash = encodeHex . B.reverse . S.encode . HK.doubleSHA256

utxo :: HK.Block -> ([STXOInfo], [UTXOInfo])
utxo block = let
    in mconcat $ inputsOutputsInfo <$> HK.blockTxns block
    where inputsOutputsInfo tx = let
            f = filter $ (/= HK.nullOutPoint) . HK.prevOutput
            txHash = HK.txHashToHex $ HK.txHash tx
            stxoInfo txIn =
              STXOInfo { txHashTarget = HK.txHashToHex $ HK.outPointHash $ HK.prevOutput txIn 
                        , stxHash = txHash
                        , stxoOutIndex = HK.outPointIndex $ HK.prevOutput txIn
                        }
            utxoInfo txOutIndex txOut = 
              UTXOInfo { txHash = txHash
                       , utxoPubKeyScriptHash = scriptOutputHash $ HK.scriptOutput txOut
                       , utxoOutIndex = fromIntegral txOutIndex
                       , outValue = HK.outValue txOut
                       }
            in (stxoInfo <$> (f $ HK.txIn tx), utxoInfo `imap` HK.txOut tx)

actualBTCHeight :: Config -> IO BlockHeight
actualBTCHeight cfg = fromIntegral <$> btcNodeClient cfg getBlockCount

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO ()
bTCBlockScanner env blockHeightToScan = do
    let cfg = envConfig env
    blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
    blockM <- (btcNodeClient cfg $ flip getBlockRaw blockHash)
    let (stxo, txo) = utxo . btcBlock $ fromMaybe (error "") blockM
        f x  = runDbQuery (envPool env) $ insertSTXO x
    runDbQuery (envPool env) $ insertUTXO txo
    forM_ stxo f
    runDbQuery (envPool env) $ updateScannedHeight BTC blockHeightToScan
    pure ()