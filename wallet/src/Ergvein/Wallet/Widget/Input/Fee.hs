
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Widget.Input.Fee(
      feeSelectionWidgetBtc
    , feeSelectionWidgetErg
  ) where

import Data.Bifunctor (second)
import Data.Either (fromLeft)
import Data.Map.Strict (Map)
import Data.Text
import Data.Word

import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate

import qualified Data.Map.Strict as M

feeModeToAttr :: FeeMode -> Map AttributeName (Maybe Text)
feeModeToAttr = \case
  FeeModeLow -> "disabled" =: Just "disabled"
  FeeModeMid -> "disabled" =: Just "disabled"
  FeeModeHigh -> "disabled" =: Just "disabled"
  FeeModeManual -> "disabled" =: Nothing

feeModeToRateText :: Currency -> Map Currency FeeBundle -> FeeMode -> Maybe Text
feeModeToRateText cur fees mode = case mode of
  FeeModeLow -> showt <$> getFeeRateByLvl FeeCheap
  FeeModeMid -> showt <$> getFeeRateByLvl FeeModerate
  FeeModeHigh -> showt <$> getFeeRateByLvl FeeFast
  FeeModeManual -> Nothing
  where
    getFeeRateByLvl lvl = fmap (fst . extractFee lvl) (M.lookup cur fees)

feeModeDropdown :: MonadFront t m => FeeMode -> m (Dynamic t FeeMode)
feeModeDropdown initFeeMode = do
  langD <- getLanguage
  l <- sampleDyn langD
  let feeModeOptionsD = constDyn $ M.fromList $ (\feeLevel -> (feeLevel, localizedShow l feeLevel)) <$> [minBound .. maxBound]
      ddnCfg = DropdownConfig {
          _dropdownConfig_setValue   = never
        , _dropdownConfig_attributes = constDyn ("class" =: "mb-0")
        }
  dp <- dropdown initFeeMode feeModeOptionsD ddnCfg
  pure $ _dropdown_value dp

-- | Btc fee selector
feeSelectionWidgetBtc :: forall t m l . (MonadFront t m, LocalizedPrint l)
  => l                                          -- ^ Label
  -> Maybe (FeeMode, Word64)                    -- ^ Inital mode and value
  -> Maybe Rational                             -- ^ Previous value (used for RBF)
  -> Event t ()                                 -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (FeeMode, Word64)))
feeSelectionWidgetBtc lbl minit mPrevRate submitE = divClass "fee-input" $ mdo
  feesD <- getFeesD
  initFees <- sampleDyn feesD
  feeModeE <- updated <$> holdUniqDyn feeModeD
  -- Be careful with feeRateErrsD, it must be declared before labeledTextFieldWithBtnsAndSelector.
  -- Otherwise you can get deadlock.
  feeRateErrsD <- holdDyn [] $ ffor (current validatedRateD `tag` submitE) (fromLeft [])
  let getInitFeeRateByLvl lvl = fmap (fst . extractFee lvl) (M.lookup BTC initFees)
      (initFeeMode, mInitFeeRate) = maybe (FeeModeMid, getInitFeeRateByLvl FeeModerate) (second Just) minit
      initFeeRateText = maybe "" showt mInitFeeRate
      modifyAttrsE = feeModeToAttr <$> feeModeE
      setValE = attachPromptlyDynWithMaybe (feeModeToRateText BTC) feesD feeModeE
      validatedRateD = toEither . validateFeeRate BTC (floor <$> mPrevRate) <$> feeRateD
      rateD = eitherToMaybe <$> validatedRateD
      selectedD = ffor2 rateD feeModeD (,)
  (feeRateD, _, feeModeD) <- labeledTextFieldWithBtnsAndSelector lbl initFeeRateText M.empty [] (feeModeDropdown initFeeMode) setValE modifyAttrsE feeRateErrsD
  networkHoldDyn $ ffor selectedD $ \case
    (Just feeRate, FeeModeManual) -> parClass "fee-rate-msg" (localizedText   FSFee                  ) >> pure (Just (FeeModeManual, feeRate))
    (Just feeRate, FeeModeLow   ) -> parClass "fee-rate-msg" (localizedText $ FSRateDesc FeeCheap    ) >> pure (Just (FeeModeLow,    feeRate))
    (Just feeRate, FeeModeMid   ) -> parClass "fee-rate-msg" (localizedText $ FSRateDesc FeeModerate ) >> pure (Just (FeeModeMid,    feeRate))
    (Just feeRate, FeeModeHigh  ) -> parClass "fee-rate-msg" (localizedText $ FSRateDesc FeeFast     ) >> pure (Just (FeeModeHigh,   feeRate))
    (Nothing     , FeeModeManual) ->                                                                      pure Nothing
    (Nothing     , _            ) -> parClass "fee-rate-msg" (localizedText   FSNoFees)                >> pure Nothing

-- | Ergo fee selector
feeSelectionWidgetErg :: forall t m l . (MonadFront t m, LocalizedPrint l)
  => l                             -- ^ Label
  -> Maybe (FeeMode, Word64)       -- ^ Inital mode and value
  -> Event t ()                    -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (FeeMode, Word64)))
feeSelectionWidgetErg = undefined
