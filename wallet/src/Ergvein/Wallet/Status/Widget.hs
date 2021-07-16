{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Status.Widget(
    statusBarWidget
  , restoreStatusWidget
  , restoreStatusDebugWidget
  , currencyStatusWidget
  ) where

import Data.Maybe (fromMaybe)
import Data.Time
import Numeric

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Widget.Balance
import Sepulcas.Elements.Markup

import qualified Data.Text as T

statusDisplayTime :: NominalDiffTime
statusDisplayTime = 5

currencyStatusWidget :: MonadFront t m => Currency -> m ()
currencyStatusWidget cur = divClass "sync-widget-wrapper" $ do
  statD <- fmap _walletStatus'normal <$> getWalletStatus cur
  localizedDynText statD

statusBarWidget :: MonadFront t m => Bool -> Currency -> m ()
statusBarWidget isVerbose cur = divClass "sync-widget-wrapper" $ do
  statD <- getWalletStatus cur
  balD <- fiatBalanceWidget cur
  rateD <- fiatRateWidget cur
  void $ networkHoldDyn $ ffor (zipDynWith (,) balD rateD) $ \case
    (Right Nothing, Right Nothing) -> void $ networkHoldDyn $ renderStatus . _walletStatus'normal <$> statD
    (eFiatBalance, eFiatRate) -> mdo
      let updE = updated statD
      let renderE = leftmost [Just . _walletStatus'normal <$> updE, Nothing <$ tE]
      tE <- networkHoldE (fiatWidget eFiatBalance eFiatRate) $ ffor renderE $ \case
        Nothing -> fiatWidget eFiatBalance eFiatRate
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

fiatWidget :: MonadFront t m => Either ExchangeRatesError (Maybe Text) -> Either ExchangeRatesError (Maybe Text) -> m (Event t ())
fiatWidget (Left _) (Left _) = do
  localizedText ExchangeRatesUnavailable
  pure never
fiatWidget eBal eRate = do
  let balanceText = case eBal of
        Left e -> localizedText e
        Right mBal -> text $ fromMaybe "" mBal
      rateText = case eRate of
        Left e -> localizedText e
        Right mRate -> text $ fromMaybe "" mRate
  balanceText
  text " "
  rateText
  pure never

restoreStatusWidget :: MonadFront t m => Currency -> m ()
restoreStatusWidget cur = do
  statD <- getWalletStatus cur
  let restoreStageD = _walletStatusRestore'stage . _walletStatus'restore <$> statD
      restoreProgressD = _walletStatusRestore'progress . _walletStatus'restore <$> statD
  h3 $ localizedText RPSInProgress
  par $ localizedDynText restoreStageD
  h3 $ localizedDynText $ showPercents <$> restoreProgressD

showPercents :: Maybe Double -> Text
showPercents = maybe "0.00%" (\p -> T.pack (showFFloat (Just 2) p "") <> "%")

-- TODO: add some more useful info
restoreStatusDebugWidget :: MonadFront t m => Currency -> m ()
restoreStatusDebugWidget cur = do
  balanceD <- balanceTitleWidget cur
  par $ dynText balanceD
  pure ()
