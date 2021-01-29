-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Widget.FeeSelector(
    btcFeeSelectionWidget
  ) where

import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Word
import Text.Read

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Fee
import Ergvein.Wallet.Monad

import qualified Data.Map.Strict as M
import qualified Data.Text as T

manualFeeSelector :: MonadFront t m => Text -> Event t Text -> Event t (Map AttributeName (Maybe Text)) -> m (Dynamic t (Maybe Word64))
manualFeeSelector text setValE attrsE = (fmap . fmap) (maybe Nothing Just . readMaybe . T.unpack) . divClass "fee-widget-input" . (textFieldAttrNoLabel attrsE setValE) $ text

feeModeToAttr :: BTCFeeMode -> Map AttributeName (Maybe Text)
feeModeToAttr = \case
  BFMLow -> "disabled" =: (Just "disabled")
  BFMMid -> "disabled" =: (Just "disabled")
  BFMHigh -> "disabled" =: (Just "disabled")
  BFMManual -> "disabled" =: Nothing

feeModeToRateText :: Map Currency FeeBundle -> BTCFeeMode -> Maybe Text
feeModeToRateText fees mode = case mode of
  BFMLow -> showt <$> getFeeRateByLvl FeeCheap
  BFMMid -> showt <$> getFeeRateByLvl FeeModerate
  BFMHigh -> showt <$> getFeeRateByLvl FeeFast
  BFMManual -> Nothing
  where
    getFeeRateByLvl lvl = maybe Nothing (Just . fst . extractFee lvl) (M.lookup BTC fees)

-- | Btc fee selector
btcFeeSelectionWidget :: forall t m l . (MonadFront t m, LocalizedPrint l)
  => l                                          -- ^ Label
  -> Maybe (BTCFeeMode, Word64)                 -- ^ Inital mode and value
  -> Event t ()                                 -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (BTCFeeMode, Word64)))
btcFeeSelectionWidget lbl minit sendE = do
  feesD <- getFeesD
  initFees <- sampleDyn feesD
  let getInitFeeRateByLvl lvl = maybe Nothing (Just . fst . extractFee lvl) (M.lookup BTC initFees)
      (initFeeMode, mInitFeeRate) = maybe (BFMMid, getInitFeeRateByLvl FeeModerate) (second Just) minit
      initFeeRateText = maybe "" showt mInitFeeRate
  divClass "fee-widget ta-l" $ do
    el "label" $ localizedText lbl
    selectedD <- row $ mdo
      feeRateD <- column67 $ do
        feeModeE <- updated <$> holdUniqDyn feeModeD
        let manualSelectorDisabledE = feeModeToAttr <$> feeModeE
            setValE = attachPromptlyDynWithMaybe feeModeToRateText feesD feeModeE
        manualFeeSelector initFeeRateText setValE manualSelectorDisabledE
      feeModeD <- column33 $ do
        langD <- getLanguage
        l <- sampleDyn langD
        let feeModeOptionsD = constDyn $ M.fromList $ (\feeLevel -> (feeLevel, localizedShow l feeLevel)) <$> [minBound .. maxBound]
        feeModeDropdown <- dropdown initFeeMode feeModeOptionsD def
        pure $ _dropdown_value feeModeDropdown
      pure $ ffor2 feeRateD feeModeD (,)
    divClass "fee-descr" $ widgetHoldDyn $ ffor selectedD $ \case
      (Just feeRate, BFMManual) -> el "label" (localizedText $ FSFee                  ) >> pure (Just (BFMManual, feeRate))
      (Just feeRate, BFMLow)    -> el "label" (localizedText $ FSRateDesc FeeCheap    ) >> pure (Just (BFMLow,    feeRate))
      (Just feeRate, BFMMid)    -> el "label" (localizedText $ FSRateDesc FeeModerate ) >> pure (Just (BFMMid,    feeRate))
      (Just feeRate, BFMHigh)   -> el "label" (localizedText $ FSRateDesc FeeFast     ) >> pure (Just (BFMHigh,   feeRate))
      (Nothing, BFMManual)      -> elClass "label" "lbl-red" (localizedText FSInvalid)  >> pure Nothing
      (Nothing, _)              -> el "label" (localizedText FSNoFees)                  >> pure Nothing
