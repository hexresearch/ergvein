{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Widget.Input.Ergo.Fee(
    feeSelectionWidget
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

feeModeToAttr :: FeeMode -> Map AttributeName (Maybe Text)
feeModeToAttr = \case
  FeeModeLow -> "disabled" =: Just "disabled"
  FeeModeMid -> "disabled" =: Just "disabled"
  FeeModeHigh -> "disabled" =: Just "disabled"
  FeeModeManual -> "disabled" =: Nothing

feeModeToRateText :: Map Currency FeeBundle -> FeeMode -> Maybe Text
feeModeToRateText fees mode = case mode of
  FeeModeLow -> showt <$> getFeeRateByLvl FeeCheap
  FeeModeMid -> showt <$> getFeeRateByLvl FeeModerate
  FeeModeHigh -> showt <$> getFeeRateByLvl FeeFast
  FeeModeManual -> Nothing
  where
    getFeeRateByLvl lvl = fmap (fst . extractFee lvl) (M.lookup ERGO fees)

-- | Ergo fee selector
feeSelectionWidget :: forall t m l . (MonadFront t m, LocalizedPrint l)
  => l                                          -- ^ Label
  -> Maybe (FeeMode, Word64)                    -- ^ Inital mode and value
  -> Event t ()                                 -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (FeeMode, Word64)))
feeSelectionWidget lbl minit submitE = do
  feesD <- getFeesD
  initFees <- sampleDyn feesD
  let getInitFeeRateByLvl lvl = fmap (fst . extractFee lvl) (M.lookup ERGO initFees)
      (initFeeMode, mInitFeeRate) = maybe (FeeModeMid, getInitFeeRateByLvl FeeModerate) (second Just) minit
      initFeeRateText = maybe "" showt mInitFeeRate
      initInputIsDisabled = initFeeMode /= FeeModeManual
  divClass "fee-input" $ do
    el "label" $ localizedText lbl
    selectedD <- row $ mdo
      feeRateD <- column67 $ mdo
        feeModeE <- updated <$> holdUniqDyn feeModeD
        let modifyAttrsE = feeModeToAttr <$> feeModeE
            setValE = attachPromptlyDynWithMaybe feeModeToRateText feesD feeModeE
        feeRateErrsD :: Dynamic t (Maybe [VError ()]) <- holdDyn Nothing $ ffor (current validatedRateD `tag` submitE) eitherToMaybe'
        selectedRateD <- manualFeeSelector initFeeRateText initInputIsDisabled setValE modifyAttrsE feeRateErrsD
        let validatedRateD = toEither . validateFeeRate Nothing . T.unpack <$> selectedRateD
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
      (Just feeRate, FeeModeManual) -> parClass "mb-0" (localizedText   FSFee                  ) >> pure (Just (FeeModeManual, feeRate))
      (Just feeRate, FeeModeLow)    -> parClass "mb-0" (localizedText $ FSRateDesc FeeCheap    ) >> pure (Just (FeeModeLow,    feeRate))
      (Just feeRate, FeeModeMid)    -> parClass "mb-0" (localizedText $ FSRateDesc FeeModerate ) >> pure (Just (FeeModeMid,    feeRate))
      (Just feeRate, FeeModeHigh)   -> parClass "mb-0" (localizedText $ FSRateDesc FeeFast     ) >> pure (Just (FeeModeHigh,   feeRate))
      (Nothing, FeeModeManual)      ->                                                              pure Nothing
      (Nothing, _)                  -> parClass "mb-0" (localizedText FSNoFees)                  >> pure Nothing
