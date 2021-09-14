{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    setupPasswordPage
  , setupLoginPage
  , setupPatternPage
  , changePasswordWidget
  ) where

import Control.Monad.Except
import Data.Maybe
import Data.Traversable (for)
import Reflex.Localize
import Text.Read

import Ergvein.Crypto.Keys (Mnemonic)
import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Localize.Password
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.PinCode
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Dropdown
import Sepulcas.Validate

import qualified Data.Text as T

setupBtcStartingHeight :: MonadFrontBase t m => m (Dynamic t BlockHeight)
setupBtcStartingHeight = do
  divClass "password-setup-descr" $ h5 $ localizedText SHSDescr
  divClass "setup-password" $ form $ fieldset $ mdo
    let defHeight = filterStartingHeight BTC
    hD <- textField SHSLabel $ showt defHeight
    let parseE = ffor (updated hD) $ \v -> case readMaybe (T.unpack v) of
          Nothing -> Left SHSParseError
          Just h -> if h < 0 then Left SHSNonNegError else Right h
    let hE = fmapMaybe (either (const Nothing) Just ) parseE
    unless isTestnet $
      void $ networkHold (pure ()) $ ffor parseE $
        divClass "validate-error" . localizedText . either id SHSEstimate
    holdDyn defHeight hE

setupPasswordPage :: MonadFrontBase t m => WalletSource -> Bool -> Maybe DerivPrefix -> Mnemonic -> [Currency] -> Maybe Text -> m ()
setupPasswordPage wt seedBackupRequired mpath mnemonic curs mlogin = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    existingWalletNames <- listStorages
    (_, pathD, heightD, logPassE) <- divClass "setup-password" $ form $ fieldset $ mdo
      p1D <- passFieldWithEye PWSPassword
      p2D <- passFieldWithEye PWSRepeat
      lpE <- validateEvent $ poke btnE $ const $ runExceptT $ do
        p1 <- sampleDyn p1D
        p2 <- sampleDyn p2D
        l  <- sampleDyn loginD
        check PWSEmptyLogin $ not $ T.null l
        check PWSNoMatch $ p1 == p2
        pure (l,p1)
      (loginD, pathD, heightD) <- dropdownContainer PWSMoreOptions PWSLessOptions (constDyn True) $ do
        loginD_ <- textFieldAttr PWSLogin ("placeholder" =: "my wallet name") $ fromMaybe (nameProposal existingWalletNames) mlogin
        pathD_ <- setupDerivPrefix curs mpath
        heightD_ <- case wt of
          WalletGenerated -> pure 0
          WalletRestored -> setupBtcStartingHeight
        pure (loginD_, pathD_, heightD_)
      btnE <- submitSetBtn
      pure (loginD, pathD, heightD, lpE)
  let goE = poke logPassE $ \(l, pass) -> do
        p <- sampleDyn pathD
        h <- sampleDyn heightD
        pure (l,pass,p,h)
  void $ nextWidget $ ffor goE $ \(login, pass, path, height) -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt seedBackupRequired mnemonic curs login pass (Just path) height True
        else performAuth wt seedBackupRequired mnemonic curs login pass (Just path) height True
    , retractablePrev = if pass == ""
        then Just $ pure $ setupPasswordPage wt seedBackupRequired (Just path) mnemonic curs (Just login)
        else Nothing
    }

setupPinCodePage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> m ()
setupPinCodePage wt seedBackupRequired mpath mnemonic login curs startingHeight = wrapperSimpleGeneric headerWidgetOnlyBackBtn "pincode-page" False $ divClass "pincode-widget" $ mdo
  let thisWidget = Just $ pure $ setupPinCodePage wt seedBackupRequired mpath mnemonic login curs startingHeight
  divClass "pincode-widget-title mt-2" $ do
    h4 $ localizedText PinCodePSTitle
    h5 $ localizedText PinCodePSPinCodeLengthRange
  inputD <- foldDyn (pinCodeFoldFunc PinCodeSetup) [] actE
  divClass "pincode-widget-dots-wrapper mb-2" $ do
    pinCodeDots (length <$> inputD)
    void $ divClass "pincode-widget-errors" $ simpleList errsD displayError
  actE <- numPadWidget PinCodeSetup
  errsD <- holdDyn [] tooShortErrE
  let submitE = ffilter (== NumPadSubmit) actE
      passD = T.concat . map showt <$> inputD
      passE = ffilter (\t -> T.length t >= minPinCodeLength) $ tagPromptlyDyn passD submitE
      tooShortErrE = [PinCodePSTooShortError] <$ ffilter (\t -> T.length t < minPinCodeLength) (tagPromptlyDyn passD submitE)
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = confirmPinCodePage pass wt seedBackupRequired mpath mnemonic login curs startingHeight
    , retractablePrev = thisWidget
    }

confirmPinCodePage :: MonadFrontBase t m
  => Text
  -> WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> m ()
confirmPinCodePage pass wt seedBackupRequired mpath mnemonic login curs startingHeight = wrapperSimpleGeneric headerWidgetOnlyBackBtn "pincode-page" False $ divClass "pincode-widget" $ mdo
  let thisWidget = Just $ pure $ confirmPinCodePage pass wt seedBackupRequired mpath mnemonic login curs startingHeight
  divClass "pincode-widget-title mt-2" $ do
    h4 $ localizedText PinCodePSConfirm
  let pinCodeLength = T.length pass
  inputD <- foldDyn (pinCodeFoldFunc (PinCodeConfirm pinCodeLength)) [] actE
  divClass "pincode-widget-dots-wrapper mb-2" $ do
    confirmPinCodeDots pinCodeLength (length <$> inputD)
    void $ divClass "pincode-widget-errors" $ simpleList errsD displayError
  actE <- numPadWidget $ PinCodeConfirm pinCodeLength
  errsD <- holdDyn [] matchErrE
  let passD = T.concat . map showt <$> inputD
      matchErrE = [PinCodePSConfirmationError] <$ ffilter (\p -> T.length p == pinCodeLength && p /= pass) (updated passD)
  passE <- delay 0.2 $ ffilter (== pass) $ updated passD
  void $ nextWidget $ ffor passE $ \confirmedPass -> Retractable {
      retractableNext = performAuth wt seedBackupRequired mnemonic curs login confirmedPass mpath startingHeight False
    , retractablePrev = thisWidget
    }

confirmEmptyPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Mnemonic
  -> [Currency]
  -> Text
  -> Password
  -> Maybe DerivPrefix
  -> BlockHeight
  -> Bool
  -> m ()
confirmEmptyPage wt seedBackupRequired mnemonic curs login pass mpath startingHeight isPass = wrapperSimple True $ do
  h4 $ localizedText CEPAttention
  h5 $ localizedText CEPConsequences
  divClass "fit-content ml-a mr-a" $ do
    setE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
    void $ retract =<< divClass "" (submitClass "button button-outline w-100" CEPBack)
    void $ nextWidget $ ffor setE $ const $ Retractable {
        retractableNext = performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight isPass
      , retractablePrev = Nothing
      }

performAuth :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Mnemonic
  -> [Currency]
  -> Text
  -> Password
  -> Maybe DerivPrefix
  -> BlockHeight
  -> Bool
  -> m ()
performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight isPass = do
  goE <- case wt of
    WalletGenerated -> getPostBuild
    WalletRestored -> wrapperSimple True $ do
      h3 $ localizedText RPSTrafficTitle
      elClass "h5" "overflow-wrap-bw" $ localizedText RPSTrafficWarn
      elClass "h5" "overflow-wrap-bw" $ localizedText RPSTrafficWifi
      elClass "h5" "overflow-wrap-bw" $ localizedText RPSTrafficTime
      outlineButton RPSTrafficAccept
  storageE <- performEvent $ ffor goE $ const $
    initWalletInfo English wt seedBackupRequired mpath mnemonic curs login pass startingHeight isPass
  walletInfoE <- handleDangerMsg storageE
  void $ setWalletInfo $ Just <$> walletInfoE

setupLoginPage :: MonadFrontBase t m => WalletSource -> Bool -> Maybe DerivPrefix -> Mnemonic -> [Currency] -> m ()
setupLoginPage wt seedBackupRequired mpath mnemonic curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  rec
    loginE <- setupLogin btnE
    heightD <- case wt of
      WalletGenerated -> pure 0
      WalletRestored -> setupBtcStartingHeight
    pathD <- dropdownContainer PWSMoreOptions PWSLessOptions (constDyn True) $ do
      setupDerivPrefix curs mpath
    btnE <- submitSetBtn
  let goE = poke loginE $ \l -> do
        p <- sampleDyn pathD
        h <- sampleDyn heightD
        pure (l,p,h)
  void $ nextWidget $ ffor goE $ \(l,p,h) -> Retractable {
      retractableNext = passwordTypeSelectionPage wt seedBackupRequired (Just p) mnemonic l curs h
    , retractablePrev = Just $ pure $ setupLoginPage wt seedBackupRequired (Just p) mnemonic curs
    }

data GoPage = GoPinCode | GoTextPassword | GoEmptyPassword

passwordTypeSelectionPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> m ()
passwordTypeSelectionPage wt seedBackupRequired (Just p) mnemonic login curs h = wrapperSimple True $ do
  h4 $ localizedText PasswordTypeTitle
  divClass "initial-page-options" $ do
    let thisWidget = passwordTypeSelectionPage wt seedBackupRequired (Just p) mnemonic login curs h
        items = [(GoPinCode, PasswordTypePin), (GoTextPassword, PasswordTypeText), (GoEmptyPassword, PasswordTypeEmpty)]
    goE <- fmap leftmost $ for items $ \(act, lbl) ->
      (act <$) <$> outlineButton lbl
    void $ nextWidget $ ffor goE $ \go -> Retractable {
        retractableNext = case go of
          GoPinCode -> setupPinCodePage wt seedBackupRequired (Just p) mnemonic login curs h
          GoTextPassword -> setupMobilePasswordPage wt seedBackupRequired (Just p) mnemonic login curs h
          GoEmptyPassword -> confirmEmptyPage wt seedBackupRequired mnemonic curs login "" (Just p) h True
      , retractablePrev = Just $ pure thisWidget
      }

setupPatternPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> m ()
setupPatternPage wt seedBackupRequired mpath mnemonic login curs startingHeight = wrapperSimple True $ do
  let this = Just $ pure $ setupPatternPage wt seedBackupRequired mpath mnemonic login curs startingHeight
  divClass "password-setup-title" $ h4 $ localizedText PatPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PatPSDescr
  patE <- setupPattern
  (setPassE, skipE) <- divClass "fit-content ml-a mr-a" $ do
    setPassE <- divClass "" $ submitClass "button button-outline w-100" PatPSPass
    skipE <- divClass "" $ submitClass "button button-outline w-100" CEPSkip
    pure (setPassE, skipE)
  let passE = leftmost ["" <$ skipE, patE]
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt seedBackupRequired mnemonic curs login pass mpath startingHeight False
        else performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight False
    , retractablePrev = if pass == "" then this else Nothing
    }
  void $ nextWidget $ ffor setPassE $ const $ Retractable {
      retractableNext = setupMobilePasswordPage wt seedBackupRequired mpath mnemonic login curs startingHeight
    , retractablePrev = this
    }

setupMobilePasswordPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> m ()
setupMobilePasswordPage wt seedBackupRequired mpath mnemonic login curs startingHeight = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSPassTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    passE <- setupPassword btnE
    btnE <- divClass "fit-content ml-a mr-a" $ do
      divClass "" $ submitClass "button button-outline w-100" PWSSet
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt seedBackupRequired mnemonic curs login pass mpath startingHeight True
        else performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight True
    , retractablePrev = if pass == ""
        then Just $ pure $ setupMobilePasswordPage wt seedBackupRequired mpath mnemonic login curs startingHeight
        else Nothing
    }

data ChangePasswordStrings = CPSTitle | CPSDescr | CPSOld

instance LocalizedPrint (Bool, ChangePasswordStrings) where
  localizedShow l (b, v) = case l of
    English -> let s = if b then "password" else "key" in case v of
      CPSTitle -> "Change " <> s
      CPSDescr -> "Enter the new " <> s <> " first"
      CPSOld   -> "You will have to enter the old " <> s <> " at the end"
    Russian -> let s = if b then "пароль" else "ключ" in case v of
      CPSTitle -> "Смена " <> if b then "пароля" else "ключа"
      CPSDescr -> "Введите новый " <> s
      CPSOld   -> "В конце вам понадобится ввести старый " <> s

changePasswordWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordWidget =
  if isAndroid then changePasswordMobileWidget else changePasswordDesktopWidget
{-# INLINE changePasswordWidget #-}

changePasswordDesktopWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordDesktopWidget = divClass "my-a" $ mdo
    tglD <- holdDyn False tglE
    (passE, tglE) <- fmap switchDyn2 $ networkHoldDyn $ ffor tglD $ \case
      True  -> (fmap . fmap) (False <$) confirmEmptyWidget
      False -> mdo
        changePasswordDescr True
        passE' <- setupPassword btnE
        btnE <- submitSetBtn
        let (emptyE, passE'') = splitFilter (== "") passE'
        pure (passE'', True <$ emptyE)
    pure $ (,True) <$> passE

changePasswordDescr :: (MonadLocalized t m, DomBuilder t m, PostBuild t m) => Bool -> m ()
changePasswordDescr b = do
  divClass "password-setup-title" $ h4 $ localizedText (b, CPSTitle)
  divClass "password-setup-descr" $ h5 $ localizedText (b, CPSDescr)
  divClass "password-setup-descr" $ h5 $ localizedText (b, CPSOld)

confirmEmptyWidget :: MonadFront t m => m (Event t Password, Event t ())
confirmEmptyWidget = do
  h4 $ localizedText CEPAttention
  h5 $ localizedText CEPConsequences
  divClass "fit-content ml-a mr-a" $ do
    setE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
    backE <- divClass "" (submitClass "button button-outline w-100" CEPBack)
    pure ("" <$ setE, backE)

data CPMStage = CPMPin | CPMPassword | CPMEmpty Bool

changePasswordMobileWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordMobileWidget = mdo
  login <- fmap _walletInfo'login $ sampleDyn =<< getWalletInfo
  let name = T.replace " " "_" login
  stage0 <- eitherToStage <$> retrieveValue ("meta_wallet_" <> name) False
  stageD <- holdDyn stage0 nextE
  (patE, nextE) <- fmap switchDyn2 $ networkHoldDyn $ ffor stageD $ \case
    CPMEmpty b -> do
      (passE, backE) <- confirmEmptyWidget
      pure ((,b) <$> passE, boolToStage b <$ backE)
    CPMPassword -> mdo
      changePasswordDescr True
      passE' <- setupPassword btnE
      (btnE, setPattE) <- divClass "fit-content ml-a mr-a" $ do
        btnE' <- divClass "" $ submitClass "button button-outline w-100" PWSSet
        setPattE' <- divClass "" $ submitClass "button button-outline w-100" PatPSPatt
        pure (btnE', setPattE')
      let (emptyE, passE) = splitFilter (== "") passE'
      let nxtE = leftmost [CPMEmpty True <$ emptyE, CPMPin <$ setPattE]
      pure ((, True) <$> passE, nxtE)
    CPMPin -> do
      changePasswordDescr False
      patE' <- setupPattern
      divClass "fit-content ml-a mr-a" $ do
        setPassE <- divClass "" $ submitClass "button button-outline w-100" PatPSPass
        skipE <- divClass "" $ submitClass "button button-outline w-100" CEPSkip
        let nxtE = leftmost [CPMPassword <$ setPassE, CPMEmpty False <$ skipE]
        pure ((, False) <$> patE', nxtE)
  pure patE
  where
    boolToStage b = if b then CPMPassword else CPMPin
    eitherToStage = either (const CPMPin) boolToStage
