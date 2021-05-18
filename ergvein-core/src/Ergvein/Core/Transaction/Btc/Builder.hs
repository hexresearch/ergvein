{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Btc.Builder(
    buildTx
  , buildAddrTx
) where

import Data.Text (Text)
import Data.Word (Word64)
import Network.Haskoin.Address (addressToOutput, stringToAddr)
import Network.Haskoin.Script (ScriptOutput (..), encodeOutputBS)
import Network.Haskoin.Transaction (OutPoint (..), Tx (..), TxIn (..), TxOut (..))

import Ergvein.Types.Network (Network)
import Ergvein.Types.Transaction (RbfEnabled)

import qualified Data.Text                          as T
import qualified Data.ByteString                    as B

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: RbfEnabled -> [OutPoint] -> [(ScriptOutput, Word64)] -> Either String Tx
buildTx rbfEnabled xs ys =
  mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint B.empty rbf
    rbf = if rbfEnabled then maxBound - 2 else maxBound
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
