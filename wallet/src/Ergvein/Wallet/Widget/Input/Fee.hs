
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Widget.Input.Fee(
      feeSelectionWidgetBtc
    , feeSelectionWidgetErg
  ) where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import Data.Word (Word64)

import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad

import qualified Data.Map.Strict as M

feeModeToAttr :: FeeMode -> Map AttributeName (Maybe Text)
feeModeToAttr = \case
  FeeModeLow -> "disabled" =: Just "disabled"
  FeeModeMid -> "disabled" =: Just "disabled"
  FeeModeHigh -> "disabled" =: Just "disabled"
  FeeModeManual -> "disabled" =: Nothing

getFeeRateByLvl :: Currency -> FeeLevel -> Map Currency FeeBundle -> Maybe Word64
getFeeRateByLvl cur lvl fees = fmap (fst . extractFee lvl) (M.lookup cur fees)

feeModeToRateText :: Currency -> Map Currency FeeBundle -> FeeMode -> Maybe Text
feeModeToRateText cur fees mode = case mode of
  FeeModeLow -> showt <$> getFeeRateByLvl cur FeeCheap fees
  FeeModeMid -> showt <$> getFeeRateByLvl cur FeeModerate fees
  FeeModeHigh -> showt <$> getFeeRateByLvl cur FeeFast fees
  FeeModeManual -> Nothing

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
feeSelectionWidgetBtc :: (MonadFront t m, LocalizedPrint l0, LocalizedPrint l1)
  => l0                      -- ^ Label
  -> Maybe (Word64, FeeMode) -- ^ Inital mode and value
  -> Dynamic t [l1]          -- ^ Dynamic with errors
  -> m (Dynamic t Text, Dynamic t FeeMode)
feeSelectionWidgetBtc lbl minit errsD = divClass "fee-input" $ mdo
  feesD <- getFeesD
  initFees <- sampleDyn feesD
  uniqFeeModeD <- holdUniqDyn feeModeD
  feeModeE <- updatedWithInit uniqFeeModeD
  let
    getInitFeeRateByLvl lvl = fmap (fst . extractFee lvl) (M.lookup BTC initFees)
    (mInitFeeRate, initFeeMode) = maybe (getInitFeeRateByLvl FeeModerate, FeeModeMid) (first Just) minit
    initFeeRateText = maybe "" showt mInitFeeRate
    modifyAttrsE = feeModeToAttr <$> feeModeE
    setValE = attachPromptlyDynWith
      (\fees feeMode -> fromMaybe "" $ feeModeToRateText BTC fees feeMode)
      feesD
      feeModeE
    infoD =
      ffor
        (zipDynWith (,) feesD feeModeD)
        ( \(fees, feeMode) -> case (M.lookup BTC fees, feeMode) of
            (Just _, FeeModeLow) -> FSRateDesc FeeCheap
            (Just _, FeeModeMid) -> FSRateDesc FeeModerate
            (Just _, FeeModeHigh) -> FSRateDesc FeeFast
            (_, FeeModeManual) -> FSFee
            _ -> FSNoFees
        )
  (feeRateD, _, feeModeD) <- labeledTextFieldWithBtnsAndSelector lbl initFeeRateText M.empty [] (feeModeDropdown initFeeMode) setValE modifyAttrsE errsD
  dyn_ $ ffor infoD (parClass "fee-rate-msg" . localizedText)
  pure (feeRateD, feeModeD)

-- | Ergo fee selector
feeSelectionWidgetErg :: forall t m l . (MonadFront t m, LocalizedPrint l)
  => l                             -- ^ Label
  -> Maybe (FeeMode, Word64)       -- ^ Inital mode and value
  -> Event t ()                    -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (FeeMode, Word64)))
feeSelectionWidgetErg = undefined
