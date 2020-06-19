module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
import Data.Either (isRight)
import Data.Word
import Network.Bitcoin.Api.Misc

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper

import qualified Data.Text as T
import qualified Data.Validation as V
import qualified Data.Map.Strict as M

data SendStrings
  = SendTitle Currency
  | SendBtnString
  | RecipientString
  | AmountString
  | BtnPasteString
  | BtnScanQRCode

instance LocalizedPrint SendStrings where
  localizedShow l v = case l of
    English -> case v of
      SendTitle c -> "Send " <> currencyName c
      SendBtnString -> "Send"
      RecipientString -> "Recipient"
      AmountString -> "Amount"
      BtnPasteString -> "Paste"
      BtnScanQRCode -> "Scan"
    Russian -> case v of
      SendTitle c -> "Отправить " <> currencyName c
      SendBtnString -> "Отправить"
      RecipientString -> "Получатель"
      AmountString -> "Сумма"
      BtnPasteString -> "Вставить"
      BtnScanQRCode -> "Сканировать"

sendPage :: MonadFront t m => Currency -> Maybe (EgvAddress, Rational) -> m ()
sendPage cur minit = wrapper (SendTitle cur) (Just $ pure $ sendPage cur Nothing) False $ do
  let thisWidget = Just $ pure $ sendPage cur minit
  navbarWidget cur thisWidget NavbarSend
  divClass "centered-wrapper" $ divClass "centered-content" $ divClass "send-page" $ form $ fieldset $ mdo
    recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . fst)
    recipientD <- validatedTextFieldSetVal RecipientString "" recipientErrsD resQRcodeE
    (qrE, pasteE, resQRcodeE) <- divClass "send-buttons-wrapper" $ do
      qrE <- outlineButtonWithIcon BtnScanQRCode "fas fa-qrcode fa-lg"
      openE <- delay 1.0 =<< openCamara qrE
      resQRcodeE <- waiterResultCamera openE
      pasteE <- outlineButtonWithIcon BtnPasteString "fas fa-clipboard fa-lg"
      pure (qrE, pasteE, resQRcodeE)
    amountErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . snd)
    amountD <- validatedTextField AmountString "" amountErrsD
    feeD <- btcFeeSelectionWidget
    submitE <- submitClass "button button-outline send-submit" SendBtnString
    let validationE = poke submitE $ \_ -> do
          recipient <- sampleDyn recipientD
          amount <- sampleDyn amountD
          pure (V.toEither $ validateRecipient cur (T.unpack recipient),
                V.toEither $ validateAmount $ T.unpack amount)
    pure ()

data BTCFeeMode = BFMCons | BFMEcon | BFMManual
  deriving (Eq)

instance LocalizedPrint BTCFeeMode where
  localizedShow l v = case l of
    English -> case v of
      BFMCons   -> "Conservative"
      BFMEcon   -> "Economical"
      BFMManual -> "Manual"
    Russian -> case v of
      BFMCons   -> "Консервативная"
      BFMEcon   -> "Эконом"
      BFMManual -> "Вручную"

instance LocalizedPrint FeeLevel where
  localizedShow l v = case l of
    English -> case v of
      FeeFast     -> "High"
      FeeModerate -> "Mid"
      FeeCheap    -> "Low"
    Russian -> case v of
      FeeFast     -> "Высокий"
      FeeModerate -> "Средний"
      FeeCheap    -> "Низкий"

data FeeStrings
  = FSMode
  | FSLevel
  | FSSelect
  | FSMModeDesc EstimateMode
  | FSMLevelDesc FeeLevel
  | FSMFee Word64
  | FSMInvalid
  | FSMNoFees

instance LocalizedPrint FeeStrings where
  localizedShow l v = case l of
    English -> case v of
      FSMode  -> "Fee mode"
      FSLevel -> "Fee level"
      FSSelect -> "Select fee mode and level"
      FSMModeDesc Conservative -> "Conservative estimate satisfies a longer history. More likely to be sufficient, but might miss short term drops in fees."
      FSMModeDesc Economical -> "Economical estimate satisfies a shorter history. It is usually lower than the conservative estimate but might not be sufficient for the target."
      FSMLevelDesc l -> let t = "Tx is to be accepted within next " <> showt (feeTargetBlocks BTC l) <> " blocks." in case l of
        FeeFast -> "High fee. " <> t
        FeeModerate -> "Moderate fee. " <> t
        FeeCheap -> "Low fee. " <> t
      FSMFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSMInvalid -> "Enter valid integer fee in satoshi/vbyte"
      FSMNoFees -> "Fees not found in the cache. Please enter the fee manually."
    Russian -> case v of
      FSMode  -> "Тип комиссии"
      FSLevel -> "Уровень комиссии"
      FSSelect -> "Выберите тип и уровень комиссии"
      FSMModeDesc Conservative -> "Консервативная оценка использует более долгую историю транзакций. Выше вероятность попадания в нужные рамки блоков, но может не учесть резкие снижения комисии."
      FSMModeDesc Economical -> "Экономная оценка использует короткую историю транзакций. Дешевле консервативной, но может быть недостаточной для попадания в нужные сроки."
      FSMLevelDesc l -> let t = "Ожидается принятие транзакции в течение следующих " <> showt (feeTargetBlocks BTC l) <> " блоков." in case l of
        FeeFast -> "Высокая комиссия. " <> t
        FeeModerate -> "Умеренная комиссия. " <> t
        FeeCheap -> "Низкая комиссия. " <> t
      FSMFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSMInvalid -> "Введите комиссию. Целое число, satoshi/vbyte"
      FSMNoFees -> "Уровень комиссий не найден в кэше. Пожалуйста, введите комиссию вручную."

btcFeeSelectionWidget :: forall t m . MonadFront t m => m (Dynamic t (Maybe Int))
btcFeeSelectionWidget = do
  feesD <- getFeesD
  valD <- divClass "fee-widget" $ do
    modeD <- divClass "fee-mode" $ mdo
      el "label" $ localizedText FSMode
      let modeE = leftmost [consE, econE, manE]
      modeD <- holdDyn Nothing modeE
      let attrD m = ffor modeD $ \m' -> if m' == Just m then "button button-outline btn-fee-on" else "button button-outline"
      consE <- fmap (Just BFMCons <$)   $ buttonClass (attrD BFMCons) BFMCons
      econE <- fmap (Just BFMEcon <$)   $ buttonClass (attrD BFMEcon) BFMEcon
      manE  <- fmap (Just BFMManual <$) $ buttonClass (attrD BFMManual) BFMManual
      holdUniqDyn modeD
    lvlD <- divClass "fee-level" $ fmap join $ widgetHoldDyn $ ffor modeD $ \case
      Nothing -> pure (pure Nothing)
      Just BFMManual -> (fmap . fmap) (Just . Left) manualFeeSelector
      _ -> mdo
        el "label" $ localizedText FSLevel
        let lvlE = leftmost [chpE, midE, fastE]
        lvlD' <- holdDyn Nothing lvlE
        let attrD m = ffor lvlD' $ \m' -> if m' == Just m then "button button-outline btn-fee-on" else "button button-outline"
        chpE  <- fmap (Just FeeCheap <$)    $ buttonClass (attrD FeeCheap) FeeCheap
        midE  <- fmap (Just FeeModerate <$) $ buttonClass (attrD FeeModerate) FeeModerate
        fastE <- fmap (Just FeeFast <$)     $ buttonClass (attrD FeeFast) FeeFast
        pure $ (fmap . fmap) Right lvlD'
    pure $ do
      mmode <- modeD
      mlvl  <- lvlD
      pure $ case (,) <$> mmode <*> mlvl of
        Nothing -> Nothing
        Just (BFMManual, Left v) -> Just (Left v)
        Just (BFMCons, Right v) -> Just $ Right (Conservative, v)
        Just (BFMEcon, Right v) -> Just $ Right (Economical, v)
        _ -> Nothing
  feeD <- divClass "fee-descr" $ widgetHoldDyn $ ffor valD $ \case
    Nothing -> el "label" $ localizedText FSSelect >> pure (pure Nothing)
    Just (Left Nothing) -> el "label" $ localizedText FSMInvalid >> pure (pure Nothing)
    Just (Left (Just f)) -> (el "div" $ localizedText $ FSMFee $ fromIntegral f) >> pure (pure $ Just f)
    Just (Right (md, lvl)) -> do
      el "div" $ localizedText $ FSMModeDesc md
      fD <- el "div" $ widgetHoldDyn $ ffor feesD $ \fm -> case M.lookup BTC fm of
        Nothing -> localizedText FSMNoFees >> pure Nothing
        Just fb -> let
          (c,e) = extractFee lvl fb
          f = case md of
            Conservative -> c
            Economical -> e
          in (localizedText $ FSMFee f) >> pure (Just $ fromIntegral f)
      el "div" $ localizedText $ FSMLevelDesc lvl
      pure fD
  pure $ join feeD

manualFeeSelector :: MonadFront t m => m (Dynamic t (Maybe Int))
manualFeeSelector = pure $ pure Nothing
