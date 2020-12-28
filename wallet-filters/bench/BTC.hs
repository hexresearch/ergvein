{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Criterion.Main
import Data.Maybe
import Data.Vector (Vector)
import Ergvein.Filters.Btc
import Ergvein.Text
import Network.Haskoin.Address
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Transaction
import System.Random

import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Ergvein.Filters.Btc.Mutable as M

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
    env makeBlockEnv $ \ ~(txs, block) -> let
      bhash = headerHash . blockHeader $ block
      n = V.length txs
      ptxs = V.drop (n `div` 2) txs -- txs that are in filter
      ntxs = V.take (n `div` 2) txs -- txs that not in filter
      txsf = makeBtcFilter btcTest (V.toList ptxs) block
      in bgroup "scan immutable" [
          bench "scan positive address" $ nfIO $ do
            addr <- getTxsAddress ptxs
            pure $ applyBtcFilter btcTest bhash txsf addr
        , bench "scan negative address" $ nfIO $ do
            addr <- getTxsAddress ntxs
            pure $ applyBtcFilter btcTest bhash txsf addr
        , bench "scan all address" $ nfIO $ do
            addr <- getTxsAddress txs
            pure $ applyBtcFilter btcTest bhash txsf addr
        ]
  , env makeMutBlockEnv $ \ ~(txs, block, txsf) -> let
      bhash = headerHash . blockHeader $ block
      n = V.length txs
      ptxs = V.drop (n `div` 2) txs -- txs that are in filter
      ntxs = V.take (n `div` 2) txs -- txs that not in filter
      in bgroup "scan mutable" [
          bench "scan positive address" $ nfIO $ do
            addr <- getTxsAddress ptxs
            M.applyBtcFilter btcTest bhash txsf addr
        , bench "scan negative address" $ nfIO $ do
            addr <- getTxsAddress ntxs
            M.applyBtcFilter btcTest bhash txsf addr
        , bench "scan all address" $ nfIO $ do
            addr <- getTxsAddress txs
            M.applyBtcFilter btcTest bhash txsf addr
        ]
  ]

makeBlockEnv :: IO (Vector Tx, Block)
makeBlockEnv = (,) <$> loadTransactions <*> loadBlock

makeMutBlockEnv :: IO (Vector Tx, Block, M.BtcAddrFilter)
makeMutBlockEnv = do
  txs <- loadTransactions
  block <- loadBlock
  let n = V.length txs
      ptxs = V.drop (n `div` 2) txs
  txsf <- M.makeBtcFilter btcTest (V.toList ptxs) block
  pure (txs, block, txsf)

loadTransactions :: IO (Vector Tx)
loadTransactions = do
  cnt <- T.readFile "./test-transactions"
  pure $ V.fromList $ fmap loadTx $ T.lines cnt
  where
    loadTx = either error id . S.decode @Tx . hex2bs

loadBlock :: IO Block
loadBlock = do
  cnt <- T.readFile "./test-block"
  pure $ either error id . S.decode @Block . hex2bs . T.filter (/= '\n') $ cnt

getTxsAddress :: Vector Tx -> IO SegWitAddress
getTxsAddress txs = do
  addrs <- fmap (V.mapMaybe id) $ traverse getTxAddress txs
  if null addrs then fail "No segwit addresses!" else do
    i <- randomRIO (0, V.length addrs-1)
    pure $ addrs V.! i

getTxAddress :: Tx -> IO (Maybe SegWitAddress)
getTxAddress tx = do
  let addrs = catMaybes $ fmap getSegWitAddr $ txOut tx
  if null addrs then pure Nothing else do
    i <- randomRIO (0, length addrs-1)
    pure $ Just $ addrs !! i
