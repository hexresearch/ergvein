module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Data.ByteString (ByteString)
import Ergvein.Crypto.Address
import Ergvein.Crypto.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Crypto.SHA256
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Transaction (PubKeyScriptHash, BlockHeight)
import Ergvein.Wallet.Alert (handleDangerMsg)
import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Client
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Loading
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Password
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Storage.Constants
import Network.Haskoin.Block (Block, genesisBlock)

import Reflex.Dom.Main (mainWidgetWithCss)

import qualified Data.IntMap.Strict    as MI
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T

frontend :: MonadFrontBase t m => m ()
frontend = do
  logWrite "Frontend started"
  alertHandlerWidget
  loadingWidget
  askPasswordModal
  logWriter =<< fmap fst getLogsTrigger
  logWrite "Entering initial page"
  void $ retractStack initialPage `liftAuth` (scanKeys >> retractStack balancesPage)

scanKeys :: MonadFront t m => m ()
scanKeys = do
  pubKeys <- getPublicKeys
  traverse_ (scanKeysCurrency pubKeys) allCurrencies

scanKeysCurrency :: MonadFront t m => M.Map Currency EgvPubKeyсhain -> Currency -> m ()
scanKeysCurrency pubKeys currency = do
  logWrite $ "Key scanning for " <> (showt currency)
  case M.lookup currency pubKeys of
      Nothing -> fail $ (show currency) ++ " public storage not found"
      Just pubKeys -> traverse_ (scanKeysPurpose pubKeys currency) [External, Internal]

scanKeysPurpose :: MonadFront t m => EgvPubKeyсhain -> Currency -> KeyPurpose -> m ()
scanKeysPurpose pubKeys currency keyPurpose = mdo
  gapD <- holdDyn 0 gapE
  nextKeyIndexD <- holdDyn 0 nextKeyIndexE
  buildE <- getPostBuild
  nextE' <- delay 0 nextE
  filterAddressE <- filterAddress nextAddrE
  getBlockE <- getBlocks filterAddressE
  storedE <- storeNewTransactions getBlockE
  let masterPubKey = egvPubKeyсhain'master pubKeys
      nextE = leftmost [newE, buildE]
      newE = flip push storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap >= gapLimit then Nothing else Just ()
      gapE = traceEvent "gap" <$> flip pushAlways storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap < gapLimit then gap + 1 else 0
      nextAddrE = traceEventWith (("address derived: " ++) . T.unpack . egvAddrToString . egvAddress . snd) <$> flip push nextE' $ \_ -> do
        gap <- sample . current $ gapD
        nextKeyIndex <- sample . current $ nextKeyIndexD
        pure $ if gap >= gapLimit then Nothing else Just $ generateNextAddr masterPubKey keyPurpose nextKeyIndex
      nextKeyIndexE = traceEvent "next key index" <$> flip pushAlways nextAddrE $ \_ -> do
        nextKeyIndex <- sample . current $ nextKeyIndexD
        pure $ nextKeyIndex + 1
  pure ()

generateNextAddr :: EgvXPubKey -> KeyPurpose -> Int -> (Int, EgvAddress)
generateNextAddr master purpose index = (index, egvXPubKeyToEgvAddress derivedXPubKey)
  where currency = egvXPubCurrency master
        derivedXPubKey = derivePubKey master purpose (fromIntegral index)

-- FIXME
filterAddress :: MonadFront t m => Event t (Int, EgvAddress) -> m (Event t [BlockHeight])
filterAddress addrE = pure $ filterAddressMock <$> addrE
  where filterAddressMock (idx, addr) = if idx < 5 then [1] else []

-- FIXME
getBlocks :: MonadFront t m => Event t [BlockHeight] -> m (Event t [Block])
getBlocks blockHeightE = pure $ getBlocksMock <$> blockHeightE
  where getBlocksMock bhs = if null bhs then [] else [genesisBlock $ getCurrencyNetwork BTC]

-- FIXME
storeNewTransactions :: MonadFront t m => Event t [Block] -> m (Event t Int)
storeNewTransactions valE = pure $ storeNewTransactionsMock <$> valE
  where storeNewTransactionsMock blocks = if null blocks then 0 else 1
