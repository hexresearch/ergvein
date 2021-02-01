module Data.Ergo.BlockTest where

import Data.ByteString (ByteString)
import Data.Either
import Data.Text (Text, unpack)
import Data.Ergo.Autolykos
import Data.Ergo.Block
import Data.Ergo.Crypto
import Data.Ergo.Difficulty
import Data.Ergo.Modifier
import Data.Persist
import Data.Time
import Test.Tasty.HUnit

import qualified Data.ByteString.Base16 as B16

import Debug.Trace

unit_blockHeaderParse :: IO ()
unit_blockHeaderParse = do
  let mh = traceShowId $ runGet (get :: Get BlockHeader) header1
  assertBool "Block header 414474 parsed" $ isRight mh
  let Right h = mh
  let b16 = fst . B16.decode
      ecPoint :: Text -> EcPointType
      ecPoint s = either (error $ "Failed to parse public key " ++ unpack s) id . decodeEcPointType $ s
  version h @?= 1
  parentId h @?= ModifierId (b16 "8bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e")
  adProofsRoot h @?= Digest32 (b16 "4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0")
  transactionsRoot h @?= Digest32 (b16 "722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951")
  stateRoot h @?= Digest33 (b16 "6c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512")
  fromTime (timestamp h) @?= fromTime (toTime "01:49:59 29.01.2021 +0300")
  nBits h @?= Difficulty 2572389057560576
  extensionRoot h @?= Digest32 (b16 "a1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4")
  votes h @?= ParamVotes 4 3 0
  powSolution h @?= AutolykosSolution {
      minerPubKey = ecPoint "02bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3"
    , oneTimePubKey = ecPoint "026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb14"
    , nonce = b16 "0008a1d103880117"
    , distance = 35863003992655055679291741607273543535646500642591973829915050
    }

toTime :: String -> UTCTime
toTime = parseTimeOrError False defaultTimeLocale "%H:%M:%S %d.%m.%Y %z"

fromTime :: UTCTime -> String
fromTime = formatTime defaultTimeLocale "%H:%M:%S %d.%m.%Y %z"

-- | Block header on height 414474 https://explorer.ergoplatform.com/en/blocks/8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f
header1 :: ByteString
header1 = fst $ B16.decode "018bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb459516c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512d4d0c1d9f42ea1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4070923938aa61904030002bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb140008a1d1038801171a16514e604d76c516eec4124f4066e6e326b9e8d2fc5165b631aa"
