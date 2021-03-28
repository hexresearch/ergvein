{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString hiding (word8)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Short (ShortByteString)
import Data.Default
import Data.Text (Text)
import Data.Word
import Database.RocksDB
import Ergvein.Index.Server.Bitcoin.API
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Text
import Ergvein.Types.Transaction hiding (BlockHeader, BlockHash)
import Network.Haskoin.Block
import Network.Haskoin.Crypto (getHash256, doubleSHA256)
import Network.Haskoin.Transaction
import Network.Haskoin.Transaction (OutPoint(..), getTxHash)
import Network.Haskoin.Transaction hiding (TxHash, buildTx)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import qualified Data.Flat as F
import qualified Data.Persist as P
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Network.Haskoin.Transaction as HK
import qualified Network.Haskoin.Transaction as HK
import Ergvein.Index.Server.PeerDiscovery
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Serialization
import Ergvein.Text
import Data.Maybe
import Ergvein.Index.Protocol.Deserialization
import qualified Data.Attoparsec.ByteString as AP

main :: IO ()
main = do
  let v :: Version = Version
        {
          versionVersion = (2,0,0)
        , versionTime = 1616632426
        , versionNonce = 6653957167416193784
        , versionScanBlocks = VU.fromList [
            ScanBlock {
              scanBlockCurrency = BTC
            , scanBlockVersion = (1,0,0)
            , scanBlockScanHeight = 287734
            , scanBlockHeight = 676163
            }
          , ScanBlock {
              scanBlockCurrency = ERGO
            , scanBlockVersion = (1,0,0)
            , scanBlockScanHeight = 287734
            , scanBlockHeight = 0
            }
          ]
        }
  -- print v
  let msg = MFiltersResponse $ FilterResponse BTC (V.fromList vals3)
      encMsg = serializeMessage msg
      decMsg = deserializeMessage $ BL.toStrict encMsg
  print decMsg
  -- let msgBs = serializeMessage msg
  -- let msgBs' = BL.drop 4 msgBs
  -- let msg' = parseMessage MFiltersResponseType $ BL.toStrict msgBs'
  -- print $ BL.length msgBs
  -- print $ msg'


  -- let a =

deserializeMessage :: BS.ByteString -> Either String Message
deserializeMessage bs = flip AP.parseOnly bs $ messageParser . msgType =<< messageHeaderParser

serializeMessage :: Message -> BL.ByteString
serializeMessage = toLazyByteString . messageBuilder

vals3 :: [BlockFilter]
vals3 = flip fmap vals $ \(h,f) -> BlockFilter (BSS.toShort $ hex2bs h) (hex2bs f)

vals2 :: [(BlockHash, ByteString)]
vals2 = catMaybes $ flip fmap vals $ \(h,f) -> case (,) <$> (S.decode $ hex2bs h) <*> (Right $ hex2bs f) of
  Left _ -> Nothing
  Right v -> Just v

vals :: [(Text, Text)]
vals = [
    ("6f3f6e272452ff8f324bedc2dd1cedf1e62f73e46c46ec040000000000000000","1089434989e4084b0c5a707cebfbf8b4c0c54b44612d40fb330ee148b0e5ca286fa221385e9516b2a8a17800")
  , ("220cd4fe3c4142258596ecf5d75775036dd0e62e1b1b42050000000000000000","0f21965ecf14a6ebcb36135601840c5e3c0127fc69c215516a279958ec3199e7abf7f8a9aa66989480")
  , ("4d083caca45b504f205fabe915881cd4266dab9a097865010000000000000000","11950ad6a01090df1c71201594671785ced58bff9aec323367a115424189ac4e256e9389fd83290862cddb748e50")
  , ("ba7430ade693efba0dcb84eca6a2a5f2ce4b4413695d3c050000000000000000","0636e35895320fdd16531543e383c197a4")
  , ("4eb46d3aca512ec6e7444c1c8da3753cda472259f00bb6030000000000000000","09c75d209f9131a8d788c8e2b2749a79858018fc21d1407468")
  , ("7bef688b63ddb6d8d3016d38ecf880afe5c576814d9712030000000000000000","1fe0b64bacfe65d76d47848b75590e8f0433254436f30758cb42ccc013c58bbe677d44f778ef789091d05114aac7886a8d756e1e1d5876d96750639fe565064e21c336b093509d8810bc98989461ef4d726f80")
  , ("08b0928d76e1eb45698ae16a5b2cc670953d8f0ce920f7020000000000000000","199553b1c8801237e0db85813e9b9a27c66bcc7f66af1abf2736381edbd09ab4ce0bfdbf23534a56d7d4c3c845cad9f7f1cb2eaf8ca69399736aaa436e98a23ec755ac")
  , ("1001a4bbe11af0ad4d38632031294438a52c52b562be77040000000000000000","00")
  , ("41286327d2e654210a6be667d2f7a76bfb378a837470ec040000000000000000","2017059980e9bf87697e2103c7c3ddb350a67fcd092f242ccd6303f6583e4ec00918f30a88d9e7b955b120ec50600ce20d483ec00573825ee560c794cfdb2e76f8c919293570350cb579146f22cbc96ef0c85410ba")
  , ("f09ad5c3bdf617d234fbcaea4505936f8911d4e50c64cd030000000000000000","0a7b3b58e2197a81c9c5bd8d5594494752515c19b54d1cd3949b0500")
  ]
