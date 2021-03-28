{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Control.DeepSeq
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Network.Haskoin.Transaction
import Test.QuickCheck
import Test.QuickCheck.Instances

import Test.Generators
import Test.Persist
import Data.Word
import Ergvein.Types.Currency

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV
import qualified Data.Serialize             as S
import qualified Data.Persist               as P



runB :: BB.Builder -> BS.ByteString
runB = BL.toStrict . BB.toLazyByteString

runP :: P.Put () -> BS.ByteString
runP = P.runPut
--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

prop_encdec_Tx :: Tx -> Bool
prop_encdec_Tx tx = either (const False) (tx ==) $ S.decode @Tx $ P.runPut $ buildTxP tx
--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
