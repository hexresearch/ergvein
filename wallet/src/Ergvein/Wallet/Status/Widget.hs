{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Status.Widget(
    statusBarWidget
  , restoreStatusWidget
  , restoreStatusDebugWidget
  , multiCurrenctyStatusBarWidget
  ) where

import Data.Time
import Numeric
import Text.Printf
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Widget.Balance
import Sepulcas.Elements.Markup
import Reflex.ExternalRef

import qualified Data.Text as T
import qualified Data.Map.Strict as M

statusDisplayTime :: NominalDiffTime
statusDisplayTime = 5

multiCurrenctyStatusBarWidget :: MonadFront t m => m ()
multiCurrenctyStatusBarWidget = divClass "sync-widget-wrapper" $ do
  statRef <- getWalletStatusRef
  let statE = externalEvent statRef
  statD <- foldDyn mrg Nothing statE
  networkHoldDyn $ ffor statD $ \case
    Nothing -> localizedText $ _walletStatus'normal emptyWalletStatus
    Just (cur,status) -> localizedText $ CurrencyStatus cur $ _walletStatus'normal status
  pure ()
  where
    mrg :: Map Currency WalletStatus -> Maybe (Currency, WalletStatus) -> Maybe (Currency, WalletStatus)
    mrg a b = listToMaybe $ M.toList $ case b of
      Nothing -> a
      Just (cur,st) -> M.differenceWith (\x y -> if x == y then Nothing else Just x) a $ M.singleton cur st

statusBarWidget :: MonadFront t m => Bool -> Currency -> m ()
statusBarWidget isVerbose cur = divClass "sync-widget-wrapper" $ do
  statD <- getWalletStatus cur
  balD <- balanceRatedOnlyWidget cur
  void $ networkHoldDyn $ ffor balD $ \case
    Nothing -> void $ networkHoldDyn $ (renderStatus . _walletStatus'normal) <$> statD
    Just bal -> mdo
      let updE = updated statD
      let renderE = leftmost [(Just . _walletStatus'normal) <$> updE, Nothing <$ tE]
      tE <- networkHoldE (balWidget cur bal) $ ffor renderE $ \case
        Nothing -> balWidget cur bal
        Just sp -> do
          renderStatus sp
          (e,_) <- elAttr' "span" [("class", "ml-1")] $ elClass "i" "fas fa-times" $ pure ()
          let closeE = domEvent Click e
          timeoutE <- delay statusDisplayTime =<< getPostBuild
          pure $ leftmost [closeE, timeoutE]
      pure ()
  where
    renderStatus status = if isVerbose
      then localizedText $ CurrencyStatus cur status
      else localizedText status

balWidget :: MonadFront t m => Currency -> T.Text -> m (Event t ())
balWidget cur bal = do
  text bal
  rateFiatD <- (fmap . fmap) settingsRateFiat getSettingsD
  _ <- networkHoldDyn $ ffor rateFiatD $ \mf -> maybeW mf $ \f -> do
    rateD <- getRateByFiatD cur f
    void $ networkHoldDyn $ ffor rateD $ \mr -> maybeW mr $ \r -> do
      let r' = T.pack $ printf "%.2f" (realToFrac r :: Double)
      text $ " (" <> r' <> " " <> showt cur <> "/" <> showt f <> ")"
  pure never
  where
    maybeW mv f = case mv of
      Nothing -> pure ()
      Just v -> f v

restoreStatusWidget :: MonadFront t m => Currency -> m ()
restoreStatusWidget cur = do
  statD <- getWalletStatus cur
  let restoreStageD = _walletStatusRestore'stage . _walletStatus'restore <$> statD
      restoreProgressD = _walletStatusRestore'progress . _walletStatus'restore <$> statD
  h3 $ localizedText RPSInProgress
  par $ localizedDynText restoreStageD
  h3 $ localizedDynText $ showPercents <$> restoreProgressD

showPercents :: Maybe Double -> Text
showPercents mPercents = maybe "0.00%" (\p -> (T.pack $ showFFloat (Just 2) p "") <> "%") mPercents

-- TODO: add some more useful info
restoreStatusDebugWidget :: MonadFront t m => Currency -> m ()
restoreStatusDebugWidget cur = do
  balanceD <- balanceTitleWidgetSimple cur
  par $ dynText balanceD
  pure ()
