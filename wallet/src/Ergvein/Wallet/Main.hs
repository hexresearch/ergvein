module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Data.ByteString (ByteString)
import Ergvein.Crypto.Keys (derivePubKey)
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Transaction (PubKeyScriptHash)
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

import Reflex.Dom.Main (mainWidgetWithCss)

import qualified Data.Map.Strict    as M
import qualified Data.IntMap.Strict as MI

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
scanKeys = mdo
  gapD <- holdDyn 0 gapE
  buildE <- getPostBuild
  let nextE = leftmost [newE, buildE]
  nextKeyE <- performEvent $ getNextKey BTC External <$ nextE
  mresE <- getTxHashHistory $ TxHashHistoryRequest BTC <$> nextKeyE
  resE <- handleDangerMsg mresE
  validE <- validateHistory resE
  storedE :: Event t Int <- storeNewTransactions validE
  let newE = flip push storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap > gapLimit then Nothing else Just ()
      gapE = flip push storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 then Just $ gap + 1 else Nothing
  pure ()

getNextKey :: MonadStorage t m => Currency -> KeyPurpose -> m PubKeyScriptHash
getNextKey curr purpose = do
  pKeys <- getPublicKeys
  let pKeyChain = M.lookup curr pKeys
  case pKeyChain of
    Nothing -> fail "getNextKey: unknown currency"
    Just pKC -> do
      let master = egvPubKeyсhain'master pKC
          index = fromIntegral $ MI.size $ egvPubKeyсhain'external pKC
          xpk = derivePubKey master purpose index
          pk = PubKeyI (xPubKey xpk) False
          p2wpkhScript = addressToOutput $ pubKeyWitnessAddr pk
          scriptHash = encodeSHA256Hex $ doubleSHA256
      pure $ scriptHash

-- FIXME
validateHistory :: Event t TxHashHistoryResponse -> m (Event t Bool)
validateHistory respE = pure $ fmap (const True) respE

-- FIXME
storeNewTransactions :: Event t Bool -> m (Event t Int)
storeNewTransactions valE = pure $ fmap (const 0) valE

-- filterAddress :: Event t Address -> m (Event t [BlockHeight])

-- getBlock :: Event t BlockHeight -> m (Event t Block)
