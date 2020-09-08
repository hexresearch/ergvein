module Main where

import Network.Haskoin.Transaction

import Control.Monad
import Control.Applicative
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Serialize.Tx
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Types.Currency
import Data.Serialize
import Data.Word

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.Binary

main :: IO ()
main = do
  -- let bs = BL.toStrict . toLazyByteString $ buildTx testTx
  let bs = egvSerialize BTC testTx
  let res = parseOnly ptx bs
  let bs' = encode testTx
  print bs
  print bs'
  print $ bs == bs'
  let a :: Either String ([TxIn], [TxOut]) =  flip parseOnly bs $ do
        v <- anyWord32le
        void $ A.word8 0x00
        void $ A.word8 0x01
        i <- replicateList parseTxIn =<< parseVarInt
        o <- replicateList parseTxOut =<< parseVarInt
        pure (i, o)
  -- print a
  print res

replicateList :: Applicative m => m a -> Word64 -> m [a]
replicateList f n = replicateM (fromIntegral n) f

testTx' :: Tx
testTx' = Tx {
    txVersion = 1
  , txIn = [ TxIn {
          prevOutput = OutPoint {outPointHash = "1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020100", outPointIndex = 0}
        , scriptInput = ""
        , txInSequence = 1
        }]
  , txOut = [ TxOut {
          outValue = 1
        , scriptOutput = ""}]
  , txWitness = [["\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US"]]
  , txLockTime = 0}

ptx :: Parser Tx
ptx = (parseLegacyTx <* endOfInput) <|> parseWitnessTx

ptx' :: Parser Tx
ptx' = parseWitnessTx <|> parseLegacyTx


testTx :: Tx
testTx = Tx {txVersion = 1, txIn = [TxIn {prevOutput = OutPoint {outPointHash = "1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020100", outPointIndex = 4}, scriptInput = "\169", txInSequence = 4}], txOut = [TxOut {outValue = 8, scriptOutput = ""},TxOut {outValue = 7, scriptOutput = "\221\230"}], txWitness = [["\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US"]], txLockTime = 0}
