{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Ergo.Send (
    sendPageErg
  ) where

import Data.Maybe
import Data.Word

import Ergvein.Types
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Widget.Input.Amount
import Ergvein.Wallet.Widget.Input.Ergo.Fee
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
    recipientD <- divClass "mb-1" $ recipientWidget recipientInit submitE
    amountD <- divClass "mb-1" $ sendAmountWidget amountInit submitE
    feeD <- divClass "mb-1" $ feeSelectionWidget FSRate feeInit submitE
    submitE <- outlineSubmitTextIconButtonClass "w-100 mb-0" SendBtnString "fas fa-paper-plane fa-lg"
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

sendConfirmationWidget = undefined