module Data.Ergo.Block2Test where

import Data.ByteString (ByteString)
import Data.Either
import Data.Ergo.Autolykos
import Data.Ergo.Block
import Data.Ergo.Crypto
import Data.Ergo.Difficulty
import Data.Ergo.Modifier
import Data.Persist
import Data.Text (Text, unpack)
import Data.Time
import Data.Time.Clock.POSIX
import Test.Tasty.HUnit

import qualified Data.ByteString.Base16 as B16

import Debug.Trace

unit_blockHeader2Parse :: IO ()
unit_blockHeader2Parse = do
  -- | This test contains simple parser for header version 2
  -- | real header from mainnet, at 418,138, https://explorer.ergoplatform.com/en/blocks/f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39
  --

  let mh = traceShowId $ runGet (get :: Get BlockHeader) header2
  assertBool "Block header 418138 parsed" $ isRight mh
  let Right h = mh

  let version' = 2

  let height' = 418138

  let parentId' = ModifierId (b16 "7fbc70ec5913706ddef67bbcdb7700ea5f15dc709012491269c9c7eb545d720c")

  let adProofsRoot' = Digest32 (b16 "a80bbd4d69b4f017da6dd9250448ef1cde492121fc350727e755c7b7ae2988ad")

  let transactionsRoot' = Digest32 (b16 "141bf3de015c44995858a435e4d6c50c51622d077760de32977ba5412aaaae03")

  let stateRoot' = Digest33 (b16 "995c0efe63744c5227e6ae213a2061c60f8db845d47707a6bff53f9ff1936a9e13")

  let timestamp' = utcTimeFromPosixMilliseconds 1612465607426

  let extensionRoot' = Digest32 (b16 "b1457df896bba9dc962f8e42187e1ac580842f1282c8c7fb9cf9f4cd520d1c07")

  let nBits' = Difficulty 107976917

  let votes' = ParamVotes 0 0 0

  let powSolution' = AutolykosSolution {
          minerPubKey = ecPoint "0315345f1fca9445eee5df74759d4c495094bcfc82a2831b26fca6efa599b509de"
        , oneTimePubKey = wForV2
        , nonce = b16 "1b95db2168f95fda"
        , distance = dForV2
        }

  let h' = BlockHeader {
          version          = version'
        , parentId         = parentId'
        , adProofsRoot     = adProofsRoot'
        , transactionsRoot = transactionsRoot'
        , stateRoot        = stateRoot'
        , timestamp        = timestamp'
        , extensionRoot    = extensionRoot'
        , nBits            = nBits'
        , height           = height'
        , votes            = votes'
        , powSolution      = powSolution'
        }
  -- print $ B16.encode $ runPut . put $ h'

  version h @?= version'
  parentId h @?= parentId'
  adProofsRoot h @?= adProofsRoot'
  transactionsRoot h @?= transactionsRoot'
  stateRoot h @?= stateRoot'
  -- fromTime (timestamp h) @?= fromTime timestamp'
  fromTime (timestamp h) @?= fromTime (toTime "23:06:47 04.02.2021 +0400")
  -- FIXME
  -- extensionRoot h @?= extensionRoot'
  -- FIXME
  -- nBits h @?= nBits'
  height h @?= height'
  votes h @?= votes'
  hashHeaderBytes header2 @?= "f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39"

  where
      b16 = fst . B16.decode

      ecPoint :: Text -> EcPointType
      ecPoint s = either (error $ "Failed to parse public key " ++ unpack s) id . decodeEcPointType $ s

utcTimeFromPosixMilliseconds :: POSIXTime -> UTCTime
utcTimeFromPosixMilliseconds = posixSecondsToUTCTime . (/1000)

toTime :: String -> UTCTime
toTime = parseTimeOrError False defaultTimeLocale "%H:%M:%S %d.%m.%Y %z"

fromTime :: UTCTime -> String
fromTime = formatTime defaultTimeLocale "%H:%M:%S %d.%m.%Y %z"

-- | Block header from mainnet, at 418,138, https://explorer.ergoplatform.com/en/blocks/f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39
header2 :: ByteString
header2 = fst $ B16.decode "027fbc70ec5913706ddef67bbcdb7700ea5f15dc709012491269c9c7eb545d720ca80bbd4d69b4f017da6dd9250448ef1cde492121fc350727e755c7b7ae2988ad141bf3de015c44995858a435e4d6c50c51622d077760de32977ba5412aaaae03995c0efe63744c5227e6ae213a2061c60f8db845d47707a6bff53f9ff1936a9e1382a6c2f3f62eb1457df896bba9dc962f8e42187e1ac580842f1282c8c7fb9cf9f4cd520d1c07066f98d5dac219000000000315345f1fca9445eee5df74759d4c495094bcfc82a2831b26fca6efa599b509de1b95db2168f95fda"

