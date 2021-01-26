{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Widget.FeeSelector(
    btcFeeSelectionWidget
  ) where

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

data FeeSelectorStatus = FSSNoEntry | FSSNoCache | FSSNoManual | FSSManual Word64 | FSSLvl (FeeLevel, Word64)

manualFeeSelector :: MonadFront t m => Text -> m (Dynamic t FeeSelectorStatus)
manualFeeSelector = (fmap . fmap) (maybe FSSNoManual FSSManual . readMaybe . T.unpack) . el "div" . textFieldNoLabel

-- | Btc fee selector
btcFeeSelectionWidget :: forall t m . MonadFront t m
  => Maybe (BTCFeeMode, Word64)                 -- ^ Inital mode and value
  -> Event t ()                                 -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (BTCFeeMode, Word64)))
btcFeeSelectionWidget minit sendE = do
  feesD <- getFeesD
  divClass "fee-widget" $ do
    el "label" $ localizedText FSLevel
    statD <- el "div" $ mdo
      let lvlE = leftmost [lowE, midE, highE, manE]
      lvlD <- holdDyn (fst <$> minit) lvlE
      mmanD <- holdDyn (maybe "" (showt . snd) minit) $ "" <$ lvlE
      let attrD m = ffor lvlD $ \m' -> if m' == Just m
            then "button button-outline btn-fee btn-fee-on"
            else "button button-outline btn-fee"
      lowE  <- fmap (Just BFMLow <$)    $ buttonClass (attrD BFMLow)    BFMLow
      midE  <- fmap (Just BFMMid <$)    $ buttonClass (attrD BFMMid)    BFMMid
      highE <- fmap (Just BFMHigh <$)   $ buttonClass (attrD BFMHigh)   BFMHigh
      manE  <- fmap (Just BFMManual <$) $ buttonClass (attrD BFMManual) BFMManual
      fmap join $ widgetHoldDyn $ ffor lvlD $ \case
        Nothing         -> pure (pure FSSNoEntry)
        Just BFMManual  -> manualFeeSelector =<< sampleDyn mmanD
        Just BFMLow     -> pure $ extractFeeD feesD FeeCheap
        Just BFMMid     -> pure $ extractFeeD feesD FeeModerate
        Just BFMHigh    -> pure $ extractFeeD feesD FeeFast
    attrD <- holdDyn [] $ leftmost [[("class", "lbl-red")] <$ sendE, [] <$ updated statD]
    divClass "fee-descr" $ el "label" $ widgetHoldDyn $ ffor statD $ \case
      FSSNoEntry    -> elDynAttr "label" attrD (localizedText FSSelect)   >> pure Nothing
      FSSNoCache    -> elDynAttr "label" attrD (localizedText FSNoFees)   >> pure Nothing
      FSSNoManual   -> elDynAttr "label" attrD (localizedText FSInvalid)  >> pure Nothing
      FSSManual f   -> el "label" (localizedText $ FSFee f)               >> pure (Just (BFMManual, f))
      FSSLvl (l, f) -> el "label" (localizedText $ FSLevelDesc l f)       >> pure (Just $ (feeLvlToMode l, f))
  where
    extractFeeD feesD lvl = ffor feesD $
      maybe FSSNoCache (FSSLvl . (lvl,) . fromIntegral . fst . extractFee lvl) . M.lookup BTC
    feeLvlToMode lvl = case lvl of
      FeeCheap    -> BFMLow
      FeeModerate -> BFMMid
      FeeFast     -> BFMHigh