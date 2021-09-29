{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Send.Erg (
    sendPageErg
  ) where

import Data.Word

import Ergvein.Types
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Orphanage ()
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Widget.Input.Amount
import Ergvein.Wallet.Widget.Input.Fee
import Ergvein.Wallet.Widget.Input.Recipient
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

sendPageErg :: MonadFront t m => Maybe ((UnitERGO, Word64), (FeeMode, Word64), ErgAddress) -> m ()
sendPageErg mInit = mdo
  walletName <- getWalletName
  title <- localized walletName
  let navbar = if isAndroid
        then blank
        else navbarWidget ERGO thisWidget NavbarSend
      thisWidget = Just $ sendPageErg <$> retInfoD
  retInfoD <- sendWidget mInit title navbar thisWidget
  pure ()

sendWidget :: MonadFront t m
  => Maybe ((UnitERGO, Word64), (FeeMode, Word64), ErgAddress)
  -> Dynamic t Text
  -> m a
  -> Maybe (Dynamic t (m ()))
  -> m (Dynamic t (Maybe ((UnitERGO, Word64), (FeeMode, Word64), ErgAddress)))
sendWidget mInit title navbar thisWidget = wrapperNavbar False title thisWidget navbar $ divClass "send-page" $ mdo
  let amountInit = (\(x, _, _) -> x) <$> mInit
      feeInit = (\(_, x, _) -> x) <$> mInit
      recipientInit = (\(_, _, x) -> x) <$> mInit
  retInfoD <- formClass "mb-0" $ mdo
    recipientD <- divClass "mb-1" $ recipientWidget ERGO recipientInit submitE
    amountD <- divClass "mb-1" $ sendAmountWidgetErg amountInit submitE
    feeD <- divClass "mb-1" $ feeSelectionWidgetErg (FSRate ERGO) feeInit submitE
    submitE <- outlineSubmitTextIconButtonClass "w-100 mb-0" SendBtnString "material-icons-round" "send"
    let goE = flip push submitE $ \_ -> do
          mrecipient <- sampleDyn recipientD
          mamount <- sampleDyn amountD
          mfee <- sampleDyn feeD
          pure $ (,,) <$> mamount <*> mfee <*> mrecipient
    void $ nextWidget $ ffor goE $ \v@(uam, (_, fee), addr) -> Retractable {
        retractableNext = sendConfirmationWidget (uam, fee, addr)
      , retractablePrev = Just $ pure $ sendPageErg $ Just v
      }
    holdDyn mInit $ Just <$> goE
  pure retInfoD

-- | Main confirmation & sign & send widget
sendConfirmationWidget :: MonadFront t m => ((UnitERGO, Word64), Word64, ErgAddress) -> m ()
sendConfirmationWidget v = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ sendConfirmationWidget v
      navbar = if isAndroid
        then blank
        else navbarWidget ERGO thisWidget NavbarSend
  wrapperNavbar False title thisWidget navbar $ divClass "send-confirm-box" $ mdo
    stxE <- makeTxWidget v
    void $ networkHold (pure ()) $ ffor stxE $ \tx -> do
      sendE <- getPostBuild
      _ <- addOutgoingTx "sendConfirmationWidget" $ TxErg (ErgTx tx Nothing) <$ sendE
      -- TODO: save tx in storage
      -- storedE <- btcMempoolTxInserter $ tx <$ addedE

      -- TODO: implement tx broadcast
      -- void $ requestBroadcast $ ffor storedE $ const $
      --   NodeReqBtc . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ tx
      goE <- delay 1 =<< outlineButton SendBtnBack
      void $ nextWidget $ ffor goE $ const $ Retractable {
            retractableNext = balancesPage
          , retractablePrev = thisWidget
        }

makeTxWidget :: MonadFront t m =>
  ((UnitERGO, Word64), Word64, ErgAddress) ->
  m (Event t ErgTxRaw)
makeTxWidget _ = do
  buildE <- getPostBuild
  pure $ mockErgoTx <$ buildE
