{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Btc.Builder(
    buildTx
  , buildAddrTx
) where

import Data.Word
import Data.Text (Text)
import Network.Haskoin.Transaction (Tx(..), TxIn(..), TxOut(..), OutPoint(..))
import Network.Haskoin.Script (ScriptOutput(..), encodeOutputBS)
import Network.Haskoin.Address (stringToAddr, addressToOutput)

import Ergvein.Types.Transaction
import Ergvein.Types.Network (Network)

import qualified Data.Text                          as T
import qualified Data.ByteString                    as B

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: RbfEnabled -> [OutPoint] -> [(ScriptOutput, Word64)] -> Either String Tx
buildTx rbfEnabled xs ys =
  mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint B.empty rbf
    rbf = if rbfEnabled then (maxBound - 2) else maxBound
    fo (o, v)
      | v <= 2100000000000000 = return $ TxOut v $ encodeOutputBS o
      | otherwise = Left $ "buildTx: Invalid amount " ++ show v

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of recipient addresses and amounts as outputs.
buildAddrTx :: Network -> RbfEnabled -> [OutPoint] -> [(Text, Word64)] -> Either String Tx
buildAddrTx net rbfEnabled xs ys = buildTx rbfEnabled xs =<< mapM f ys
  where
    f (s, v) =
      maybe (Left ("buildAddrTx: Invalid address " <> T.unpack s)) Right $ do
        a <- stringToAddr net s
        let o = addressToOutput a
        return (o, v)
