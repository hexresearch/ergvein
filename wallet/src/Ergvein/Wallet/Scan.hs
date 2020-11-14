module Ergvein.Wallet.Scan (
    scanBtcBlocks
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List
import Data.Time.Clock.POSIX
import Data.Vector (Vector)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Log.Event
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC.Blocks
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Util
import Ergvein.Filters.Btc

import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Data.Vector                        as V
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Transaction        as HT

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found.
scanner :: MonadFront t m => m ()
scanner = do
  cursD <- getActiveCursD
  void $ widgetHoldDyn $ ffor cursD $ traverse_ scannerFor . S.toList

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found. Specific for currency.
scannerFor :: MonadFront t m => Currency -> m ()
scannerFor cur = case cur of
  BTC -> pure ()
  _ -> pure ()

-- | Check given blocks for transactions that are related to given set of keys and store txs into storage.
-- Return event that fires 'True' if we found any transaction and fires 'False' if not.
scanBtcBlocks :: MonadFront t m => Vector ScanKeyBox -> Event t [(HB.BlockHash, HB.BlockHeight)] -> m (Event t Bool)
scanBtcBlocks keys hashesE = do
  performEvent_ $ ffor hashesE $ \hs -> flip traverse_ hs $ \(_,a) -> logWrite $ showt a
  let noScanE = fforMaybe hashesE $ \bls -> if null bls then Just () else Nothing
  heightMapD <- holdDyn M.empty $ M.fromList <$> hashesE
  let rhashesE = fmap (nub . fst . unzip) $ hashesE
  _ <-logEvent "Blocks requested: " rhashesE
  blocksE <- requestBTCBlocks rhashesE
  storedBlocks <- storeBlockHeadersE "scanBtcBlocks" BTC blocksE
  let blkHeightE = current heightMapD `attach` storedBlocks
  txsUpdsE <- logEvent "Transactions got: " =<< getAddressesTxs ((\(a,b) -> (keys,a,b)) <$> blkHeightE)
  void $ insertTxsUtxoInPubKeystore "scanBtcBlocks" BTC txsUpdsE
  removeOutgoingTxs "scanBtcBlocks" BTC $ (M.elems . M.unions . V.toList . snd . V.unzip . fst) <$> txsUpdsE
  pure $ leftmost [(V.any (not . M.null . snd)) . fst <$> txsUpdsE, False <$ noScanE]

-- | Extract transactions that correspond to given address.
getAddressesTxs :: MonadFront t m
  => Event t (Vector ScanKeyBox, M.Map HB.BlockHash HB.BlockHeight, [HB.Block])
  -> m (Event t (Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate))
getAddressesTxs e = do
  psD <- getPubStorageD
  performFork $ ffor (current psD `attach` e) $ \(ps, (addrs, heights, blocks)) -> liftIO $ flip runReaderT ps $ do
    (vec, b) <- fmap V.unzip $ traverse (getAddrTxsFromBlocks heights blocks) addrs
    let (outs, ins) = V.unzip b
    let upds :: BtcUtxoUpdate = (M.unions $ V.toList outs, mconcat $ V.toList ins)
    pure (vec, upds)

-- | Gets transactions related to given address from given block.
getAddrTxsFromBlocks :: (HasPubStorage m, PlatformNatives)
  => M.Map HB.BlockHash HB.BlockHeight
  -> [HB.Block]
  -> ScanKeyBox
  -> m ((ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
getAddrTxsFromBlocks heights blocks box = do
  (txMaps, uts) <- fmap unzip $ traverse (getAddrTxsFromBlock box heights) blocks
  let (outs,ins) = unzip uts
  let upds = (M.unions outs, mconcat ins)
  pure $ ((box, M.unions txMaps), upds)

getAddrTxsFromBlock :: (HasPubStorage m, PlatformNatives)
  => ScanKeyBox
  -> M.Map HB.BlockHash HB.BlockHeight
  -> HB.Block
  -> m (M.Map TxId EgvTx, BtcUtxoUpdate)
getAddrTxsFromBlock box heights block = do
  ps <- askPubStorage
  let origtxMap = ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getAddrTxsFromBlock: BTC store does not exist") . currencyPubStorage'transactions
      newtxmap = M.fromList $ (\tx -> (mkTxId tx, BtcTx tx mheha)) <$> txs
      txmap = M.union newtxmap origtxMap
  liftIO $ flip runReaderT txmap $ do
    filteredTxs <- filterTxsForAddress addr txs
    let filteredIds = S.fromList $ mkTxId <$> filteredTxs
        filteredTxMap = M.restrictKeys newtxmap filteredIds
    utxo <- getUtxoUpdatesFromTxs mh box filteredTxs
    pure $ (filteredTxMap, utxo)
  where
    mkTxId = hkTxHashToEgv . HT.txHash
    addr = egvXPubKeyToEgvAddress $ scanBox'key box
    txs = HB.blockTxns block
    blockTime = secToTimestamp . HB.blockTimestamp $ HB.blockHeader $ block
    bhash = HB.headerHash . HB.blockHeader $ block
    mh = Just $ maybe 0 fromIntegral $ M.lookup bhash heights
    mheha = (\h -> EgvTxMeta (Just h) (Just bhash) blockTime) <$> mh
    secToTimestamp t = posixSecondsToUTCTime $ fromIntegral t
