module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Data.ByteString (ByteString)
import Ergvein.Crypto.Address
import Ergvein.Crypto.Keys (derivePubKey)
import Ergvein.Crypto.SHA256
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
scanKeys = do
  logWrite "Key scanning"
  pubKeys <- getPublicKeys
  case M.lookup BTC pubKeys of
    Nothing -> fail "BTC public storage not found"
    Just btcPubKeys -> mdo
      gapD <- holdDyn 0 gapE
      lastKeyIndexD <- holdDyn 0 lastKeyIndexE
      buildE <- getPostBuild
      nextE' <- delay 0 nextE
      storedE <- storeNewTransactions nextKeyE
      let btcMasterPubKey = egvPubKeyсhain'master btcPubKeys
          nextE = leftmost [newE, buildE]
          newE = flip push storedE $ \i -> do
            gap <- sample . current $ gapD
            pure $ if i == 0 && gap >= gapLimit then Nothing else Just ()
          gapE = traceEvent "gap" <$> flip push storedE $ \i -> do
            gap <- sample . current $ gapD
            pure $ if i == 0 && gap < gapLimit then Just $ gap + 1 else Nothing
          nextKeyE = traceEvent "derived key" <$> flip push nextE' $ \_ -> do
            gap <- sample . current $ gapD
            lastKeyIndex <- sample . current $ lastKeyIndexD
            pure $ if gap >= gapLimit then Nothing else Just $ generateNextKey btcMasterPubKey BTC External lastKeyIndex
          lastKeyIndexE = traceEvent "key index" <$> flip pushAlways nextKeyE $ \_ -> do
            lastKeyIndex <- sample . current $ lastKeyIndexD
            pure $ lastKeyIndex + 1
      pure ()

generateNextKey :: EgvXPubKey -> Currency -> KeyPurpose -> Int -> PubKeyScriptHash
generateNextKey master curr purpose index = scriptHash
  where
    extendedPubKey = egvXPubKey $ derivePubKey master purpose (fromIntegral index)
    pubKey = PubKeyI (xPubKey extendedPubKey) False
    address = pubKeyWitnessAddr pubKey
    p2wpkhScript = addressToScriptBS address
    scriptHash = encodeSHA256Hex $ doubleSHA256 p2wpkhScript
    network = getCurrencyNetwork curr

-- FIXME
storeNewTransactions :: MonadFront t m => Event t PubKeyScriptHash -> m (Event t Int)
storeNewTransactions valE = pure $ 0 <$ valE
