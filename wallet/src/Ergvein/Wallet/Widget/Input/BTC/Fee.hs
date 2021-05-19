{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Widget.Input.BTC.Fee(
    btcFeeSelectionWidget
  ) where

import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import Data.Text
import Data.Word

import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate

import qualified Data.Map.Strict as M
import qualified Data.Text as T

manualFeeSelector :: (MonadFront t m, LocalizedPrint l)
  => Text -- ^ Initial value
  -> Bool -- ^ If True then field is initially disabled
  -> Event t Text -- ^ Event that changes input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> Dynamic t (Maybe [l]) -- ^ List of errors
  -> m (Dynamic t Text)
manualFeeSelector initVal isDisabled setValE attrsE errsD = divClass "fee-input-input" $
  validatedTextFieldAttrSetValNoLabel initVal attrs setValE attrsE errsD
  where
    attrs = if isDisabled then [("disabled", "disabled"), ("class", "mb-0")] else M.singleton "class" "mb-0"

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
  -> Maybe Rational                             -- ^ Previous value (used for RBF)
  -> Event t ()                                 -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (BTCFeeMode, Word64)))
btcFeeSelectionWidget lbl minit mPrevRate submitE = do
  feesD <- getFeesD
  initFees <- sampleDyn feesD
  let getInitFeeRateByLvl lvl = fmap (fst . extractFee lvl) (M.lookup BTC initFees)
      (initFeeMode, mInitFeeRate) = maybe (BFMMid, getInitFeeRateByLvl FeeModerate) (second Just) minit
      initFeeRateText = maybe "" showt mInitFeeRate
      initInputIsDisabled = initFeeMode /= BFMManual
  divClass "fee-input" $ do
    el "label" $ localizedText lbl
    selectedD <- row $ mdo
      feeRateD <- column67 $ mdo
        feeModeE <- updated <$> holdUniqDyn feeModeD
        let modifyAttrsE = feeModeToAttr <$> feeModeE
            setValE = attachPromptlyDynWithMaybe feeModeToRateText feesD feeModeE
        feeRateErrsD :: Dynamic t (Maybe [VError ()]) <- holdDyn Nothing $ ffor (current validatedRateD `tag` submitE) eitherToMaybe'
        selectedRateD <- manualFeeSelector initFeeRateText initInputIsDisabled setValE modifyAttrsE feeRateErrsD
        let validatedRateD = toEither . validateBtcFeeRate (floor <$> mPrevRate) . T.unpack <$> selectedRateD
        pure $ eitherToMaybe <$> validatedRateD
      feeModeD <- column33 $ do
        langD <- getLanguage
        l <- sampleDyn langD
        let feeModeOptionsD = constDyn $ M.fromList $ (\feeLevel -> (feeLevel, localizedShow l feeLevel)) <$> [minBound .. maxBound]
            ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = never
              , _dropdownConfig_attributes = constDyn ("class" =: "mb-0")
              }
        feeModeDropdown <- dropdown initFeeMode feeModeOptionsD ddnCfg
        pure $ _dropdown_value feeModeDropdown
      pure $ ffor2 feeRateD feeModeD (,)
    networkHoldDyn $ ffor selectedD $ \case
      (Just feeRate, BFMManual) -> parClass "mb-0" (localizedText $ FSFee                  ) >> pure (Just (BFMManual, feeRate))
      (Just feeRate, BFMLow)    -> parClass "mb-0" (localizedText $ FSRateDesc FeeCheap    ) >> pure (Just (BFMLow,    feeRate))
      (Just feeRate, BFMMid)    -> parClass "mb-0" (localizedText $ FSRateDesc FeeModerate ) >> pure (Just (BFMMid,    feeRate))
      (Just feeRate, BFMHigh)   -> parClass "mb-0" (localizedText $ FSRateDesc FeeFast     ) >> pure (Just (BFMHigh,   feeRate))
      (Nothing, BFMManual)      ->                                                              pure Nothing
      (Nothing, _)              -> parClass "mb-0" (localizedText FSNoFees)                  >> pure Nothing
