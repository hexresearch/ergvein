{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Password(
    setupPasswordPage
  , setupLoginPage
  , changePasswordPage
  ) where

import Control.Monad.Except
import Data.Maybe
import Data.Traversable (for)
import Reflex.Localize
import Text.Read

import {-# SOURCE #-} Ergvein.Wallet.Page.Settings
import Ergvein.Crypto.Keys (Mnemonic)
import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
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

data GoPage = GoPinCode | GoTextPassword | GoEmptyPassword

data ChangePasswordStrings = CPSTitle | CPSDescr | CPSOld

instance LocalizedPrint ChangePasswordStrings where
  localizedShow l v = case l of
    English -> case v of
      CPSTitle -> "Change password"
      CPSDescr -> "Enter the new password"
      CPSOld   -> "You will have to enter the old password at the end"
    Russian -> case v of
      CPSTitle -> "Смена пароля"
      CPSDescr -> "Введите новый пароль"
      CPSOld   -> "В конце вам понадобится ввести старый пароль"

type IsTextPassword = Bool

setupBtcStartingHeight :: MonadFrontBase t m => m (Dynamic t BlockHeight)
setupBtcStartingHeight = do
  divClass "password-setup-descr" $ h5 $ localizedText SHSDescr
  divClass "setup-password" $ form $ fieldset $ mdo
    let defHeight = filterStartingHeight BTC
    hD <- labeledTextInput SHSLabel $ def & textInputConfig_initialValue .~ showt defHeight
    let parseE = ffor (updated hD) $ \v -> case readMaybe (T.unpack v) of
          Nothing -> Left SHSParseError
          Just h -> if h < 0 then Left SHSNonNegError else Right h
    let hE = fmapMaybe (either (const Nothing) Just ) parseE
    unless isTestnet $
      void $ networkHold (pure ()) $ ffor parseE $
        divClass "validate-error" . localizedText . either id SHSEstimate
    holdDyn defHeight hE

setupPasswordPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> [Currency]
  -> Maybe Text
  -> Maybe Text
  -> m ()
setupPasswordPage wt seedBackupRequired mpath mnemonic curs mlogin initmnode = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    (_, pathD, heightD, logPassE, nodeD) <- divClass "setup-password" $ form $ fieldset $ mdo
      p1D <- passField PWSPassword noMatchE
      p2D <- passField PWSRepeat noMatchE
      let noMatchE = checkPasswordsMatch btnE p1D p2D
      lpE <- validateEvent $ poke btnE $ const $ runExceptT $ do
        p1 <- sampleDyn p1D
        p2 <- sampleDyn p2D
        l  <- sampleDyn loginD
        check PWSEmptyLogin $ not $ T.null l
        check PWSNoMatch $ p1 == p2
        pure (l,p1)
      (loginD, pathD, heightD, nodeD) <- dropdownContainer PWSMoreOptions PWSLessOptions (constDyn True) $
        advancedOptionsWidget wt mpath curs mlogin initmnode
      btnE <- submitSetBtn
      pure (loginD, pathD, heightD, lpE, nodeD)
  let goE = poke logPassE $ \(l, pass) -> do
        p <- sampleDyn pathD
        h <- sampleDyn heightD
        n <- sampleDyn nodeD
        pure (l,pass,p,h,n)
  void $ nextWidget $ ffor goE $ \(login, pass, path, height, mnode) -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt seedBackupRequired mnemonic curs login pass (Just path) height True mnode
        else performAuth wt seedBackupRequired mnemonic curs login pass (Just path) height True mnode
    , retractablePrev = if pass == ""
        then Just $ pure $ setupPasswordPage wt seedBackupRequired (Just path) mnemonic curs (Just login) mnode
        else Nothing
    }

advancedOptionsWidget :: MonadFrontBase t m
  => WalletSource
  -> Maybe DerivPrefix
  -> [Currency]
  -> Maybe Text
  -> Maybe Text
  -> m (Dynamic t Text, Dynamic t DerivPrefix, Dynamic t BlockHeight, Dynamic t (Maybe Text))
advancedOptionsWidget wt mpath curs mlogin mnode = do
  existingWalletNames <- listStorages
  loginD_ <- labeledTextInput PWSLogin $ def
    & textInputConfig_initialValue .~ fromMaybe (nameProposal existingWalletNames) mlogin
    & textInputConfig_initialAttributes .~ ("placeholder" =: "my wallet name")
  pathD_ <- setupDerivPrefix curs mpath
  heightD_ <- case wt of
    WalletGenerated -> pure 0
    WalletRestored -> setupBtcStartingHeight
  nodeD <- setupCustomNode mnode
  pure (loginD_, pathD_, heightD_, nodeD)

setupPinWidget :: MonadFrontBase t m => m (Event t Password)
setupPinWidget = divClass "pincode-widget" $ mdo
  divClass "pincode-widget-title mt-2" $ do
    h4 $ localizedText PinCodePSTitle
    h5 $ localizedText PinCodePSPinCodeLengthRange
  inputD <- foldDyn (pinCodeFoldFunc PinCodeSetup) [] actE
  divClass "pincode-widget-dots-wrapper mb-2" $ do
    pinCodeDots (length <$> inputD)
    void $ divClass "pincode-widget-errors" $ simpleList errsD displayErrorDyn
  actE <- numPadWidget PinCodeSetup
  errsD <- holdDyn [] tooShortErrE
  let submitE = ffilter (== NumPadSubmit) actE
      passD = T.concat . map showt <$> inputD
      passE = ffilter (\t -> T.length t >= minPinCodeLength) $ tagPromptlyDyn passD submitE
      tooShortErrE = [PinCodePSTooShortError] <$ ffilter (\t -> T.length t < minPinCodeLength) (tagPromptlyDyn passD submitE)
  pure passE

setupPinPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> Maybe Text
  -> m ()
setupPinPage wt seedBackupRequired mpath mnemonic login curs startingHeight mnode = wrapperSimpleGeneric headerWidgetOnlyBackBtn "pincode-page" False $ do
  let thisWidget = Just $ pure $ setupPinPage wt seedBackupRequired mpath mnemonic login curs startingHeight mnode
  passE <- setupPinWidget
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = confirmPinPage pass wt seedBackupRequired mpath mnemonic login curs startingHeight mnode
    , retractablePrev = thisWidget
    }

confirmPinWidget :: MonadFrontBase t m => Password -> m (Event t Password)
confirmPinWidget pass = divClass "pincode-widget" $ mdo
  divClass "pincode-widget-title mt-2" $ do
    h4 $ localizedText PinCodePSConfirm
  let pinCodeLength = T.length pass
  inputD <- foldDyn (pinCodeFoldFunc (PinCodeConfirm pinCodeLength)) [] $ leftmost [clearE, actE]
  divClass "pincode-widget-dots-wrapper mb-2" $ do
    confirmPinCodeDots pinCodeLength (length <$> inputD)
    void $ divClass "pincode-widget-errors" $ simpleList errsD displayErrorDyn
  actE <- numPadWidget $ PinCodeConfirm pinCodeLength
  errsD <- holdDyn [] matchErrE
  let passD = T.concat . map showt <$> inputD
      matchErrE = [PinCodePSConfirmationError] <$ ffilter (\p -> T.length p == pinCodeLength && p /= pass) (updated passD)
  clearE <- delay pinCodeDelayAfterInput $ NumPadClearInput <$ matchErrE -- We need this event to clear the PIN input after a failed attempt
  passE <- delay pinCodeDelayAfterInput $ ffilter (== pass) $ updated passD
  pure passE

confirmPinPage :: MonadFrontBase t m
  => Text
  -> WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> Maybe Text
  -> m ()
confirmPinPage pass wt seedBackupRequired mpath mnemonic login curs startingHeight mnode = wrapperSimpleGeneric headerWidgetOnlyBackBtn "pincode-page" False $ do
  let thisWidget = Just $ pure $ confirmPinPage pass wt seedBackupRequired mpath mnemonic login curs startingHeight mnode
  passE <- confirmPinWidget pass
  void $ nextWidget $ ffor passE $ \confirmedPass -> Retractable {
      retractableNext = performAuth wt seedBackupRequired mnemonic curs login confirmedPass mpath startingHeight False mnode
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
  -> Maybe Text
  -> m ()
confirmEmptyPage wt seedBackupRequired mnemonic curs login pass mpath startingHeight isPass mnode = wrapperSimple True $ do
  h4 $ localizedText CEPAttention
  h5 $ localizedText CEPConsequences
  divClass "fit-content ml-a mr-a" $ do
    setE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
    void $ retract =<< divClass "" (submitClass "button button-outline w-100" CEPBack)
    void $ nextWidget $ ffor setE $ const $ Retractable {
        retractableNext = performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight isPass mnode
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
  -> Maybe Text
  -> m ()
performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight isPass mnode = do
  goE <- case wt of
    WalletGenerated -> getPostBuild
    WalletRestored -> wrapperSimple True $ do
      h3 $ localizedText RPSTrafficTitle
      elClass "h5" "overflow-wrap-bw" $ localizedText RPSTrafficWarn
      elClass "h5" "overflow-wrap-bw" $ localizedText RPSTrafficWifi
      elClass "h5" "overflow-wrap-bw" $ localizedText RPSTrafficTime
      outlineButton RPSTrafficAccept
  storageE <- performEvent $ ffor goE $ const $
    initWalletInfo English wt seedBackupRequired mpath mnemonic curs login pass startingHeight isPass mnode
  walletInfoE <- handleDangerMsg storageE
  void $ setWalletInfo $ Just <$> walletInfoE

setupLoginPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> [Currency]
  -> Maybe Text
  -> m ()
setupLoginPage wt seedBackupRequired mpath mnemonic curs initmnode = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  rec
    loginE <- setupLogin btnE
    heightD <- case wt of
      WalletGenerated -> pure 0
      WalletRestored -> setupBtcStartingHeight
    (pathD, nodeD) <- dropdownContainer PWSMoreOptions PWSLessOptions (constDyn True) $ do
      pathD' <- setupDerivPrefix curs mpath
      nodeD' <- setupCustomNode initmnode
      pure (pathD', nodeD')
    btnE <- submitSetBtn
  let goE = poke loginE $ \l -> do
        p <- sampleDyn pathD
        h <- sampleDyn heightD
        n <- sampleDyn nodeD
        pure (l,p,h,n)
  void $ nextWidget $ ffor goE $ \(l,p,h,n) -> Retractable {
      retractableNext = passwordTypeSelectionPage wt seedBackupRequired p mnemonic l curs h n
    , retractablePrev = Just $ pure $ setupLoginPage wt seedBackupRequired mpath mnemonic curs n
    }

passwordTypeSelectionPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> Maybe Text
  -> m ()
passwordTypeSelectionPage wt seedBackupRequired p mnemonic login curs h mnode = wrapperSimple True $ do
  h4 $ localizedText PasswordTypeTitle
  divClass "initial-page-options" $ do
    let thisWidget = passwordTypeSelectionPage wt seedBackupRequired p mnemonic login curs h mnode
        items = [(GoPinCode, PasswordTypePin), (GoTextPassword, PasswordTypeText), (GoEmptyPassword, PasswordTypeEmpty)]
    goE <- fmap leftmost $ for items $ \(act, lbl) ->
      (act <$) <$> outlineButton lbl
    void $ nextWidget $ ffor goE $ \go -> Retractable {
        retractableNext = case go of
          GoPinCode -> setupPinPage wt seedBackupRequired (Just p) mnemonic login curs h mnode
          GoTextPassword -> setupMobilePasswordPage wt seedBackupRequired (Just p) mnemonic login curs h mnode
          GoEmptyPassword -> confirmEmptyPage wt seedBackupRequired mnemonic curs login "" (Just p) h True mnode
      , retractablePrev = Just $ pure thisWidget
      }

setupMobilePasswordPage :: MonadFrontBase t m
  => WalletSource
  -> Bool
  -> Maybe DerivPrefix
  -> Mnemonic
  -> Text
  -> [Currency]
  -> BlockHeight
  -> Maybe Text
  -> m ()
setupMobilePasswordPage wt seedBackupRequired mpath mnemonic login curs startingHeight mnode = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSPassTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    passE <- setupPassword btnE
    btnE <- divClass "fit-content ml-a mr-a" $ do
      divClass "" $ submitClass "button button-outline w-100" PWSSet
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt seedBackupRequired mnemonic curs login pass mpath startingHeight True mnode
        else performAuth wt seedBackupRequired mnemonic curs login pass mpath startingHeight True mnode
    , retractablePrev = if pass == ""
        then Just $ pure $ setupMobilePasswordPage wt seedBackupRequired mpath mnemonic login curs startingHeight mnode
        else Nothing
    }

setNewPassword :: MonadFront t m => Event t (Password, IsTextPassword) -> m (Event t ())
setNewPassword passE = do
  walletInfoD <- getWalletInfo
  eWalletInfoE <- withWallet $ ffor passE $ \(pass, isTextPassword) prv -> do
    walletInfo <- sampleDyn walletInfoD
    encryptPrvStorageResult <- encryptPrvStorage prv pass
    case encryptPrvStorageResult of
      Left err -> pure $ Left $ CreateStorageAlert err
      Right prve -> case passwordToECIESPrvKey pass of
        Left _ -> pure $ Left GenerateECIESKeyAlert
        Right k -> pure $ Right $ (, isTextPassword) $ walletInfo
          & walletInfo'storage . storage'encryptedPrvStorage .~ prve
          & walletInfo'eciesPubKey .~ toPublic k
          & walletInfo'isPlain .~ (pass == "")
  walletInfoE <- handleDangerMsg eWalletInfoE
  when isAndroid $ performEvent_ $ ffor walletInfoE $ \(walletInfo, isTextPassword) -> do
    let fpath = "meta_wallet_" <> T.replace " " "_" (_walletInfo'login walletInfo)
    storeValue fpath isTextPassword True
  setE <- eventToNextFrame =<< setWalletInfo (fmap (Just . fst) walletInfoE)
  storeWalletNow "passwordChangePage" False setE

changePasswordPage :: MonadFront t m => m ()
changePasswordPage = if isAndroid
  then selectNewPasswordTypePage
  else setNewTextPasswordPage

setNewTextPasswordPage :: MonadFront t m => m ()
setNewTextPasswordPage = do
  let isTextPassword = True
  title <- localized CPSTitle
  let thisWidget = Just $ pure setNewTextPasswordPage
  wrapperGeneric False title thisWidget Nothing "password-widget-container" $ do
    divClass "my-a" $ mdo
      divClass "password-setup-descr" $ h4 $ localizedText CPSDescr
      divClass "password-setup-descr" $ h5 $ localizedText CPSOld
      passE <- setupPassword btnE
      btnE <- submitSetBtn
      -- Ask for confirmation if the password is empty
      let emptyPassE = ffilter T.null passE
      void $ nextWidget $ ffor emptyPassE $ const Retractable {
          retractableNext = confirmEmptyPasswordPage isTextPassword settingsPage
        , retractablePrev = thisWidget
        }
      -- Set new password if the password is not empty
      let notEmptyPassE = (, isTextPassword) <$> ffilter (not . T.null) passE
      doneE <- setNewPassword notEmptyPassE
      void $ nextWidget $ ffor doneE $ const $ Retractable{
          retractableNext = settingsPage
        , retractablePrev = thisWidget
        }

confirmEmptyPasswordPage :: MonadFront t m => IsTextPassword -> m () -> m ()
confirmEmptyPasswordPage isTextPassword nextPage = do
  title <- localized CPSTitle
  wrapper True title Nothing $ do
    h4 $ localizedText CEPAttention
    h5 $ localizedText CEPConsequences
    divClass "fit-content mx-a" $ do
      submitE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
      let passE = (T.empty, isTextPassword) <$ submitE
      doneE <- setNewPassword passE
      void $ nextWidget $ ffor doneE $ const $ Retractable {
          retractableNext = nextPage
        , retractablePrev = Nothing
        }

selectNewPasswordTypePage :: MonadFront t m => m ()
selectNewPasswordTypePage = do
  title <- localized CPSTitle
  let thisWidget = Just $ pure selectNewPasswordTypePage
  wrapper True title thisWidget $ do
    h4 $ localizedText PasswordTypeTitle
    divClass "initial-page-options" $ do
      let items = [(GoPinCode, PasswordTypePin), (GoTextPassword, PasswordTypeText), (GoEmptyPassword, PasswordTypeEmpty)]
      goE <- fmap leftmost $ for items $ \(act, lbl) ->
        (act <$) <$> outlineButton lbl
      void $ nextWidget $ ffor goE $ \go -> Retractable {
          retractableNext = case go of
            GoPinCode -> setNewPinPage
            GoTextPassword -> setNewTextPasswordPage
            GoEmptyPassword -> confirmEmptyPasswordPage True settingsPage
        , retractablePrev = thisWidget
        }

setNewPinPage :: MonadFront t m => m ()
setNewPinPage = do
  title <- localized CPSTitle
  let thisWidget = Just $ pure setNewTextPasswordPage
  wrapperGeneric False title thisWidget Nothing "pincode-page" $ do
    passE <- setupPinWidget
    void $ nextWidget $ ffor passE $ \pass -> Retractable{
        retractableNext = confirmNewPinPage pass
      , retractablePrev = thisWidget
      }

confirmNewPinPage :: MonadFront t m => Password -> m ()
confirmNewPinPage pass = do
  let isTextPassword = False
  title <- localized CPSTitle
  let thisWidget = Just $ pure $ confirmNewPinPage pass
  wrapperGeneric False title thisWidget Nothing "pincode-page" $ do
    passE <- confirmPinWidget pass
    doneE <- setNewPassword $ (, isTextPassword) <$> passE
    void $ nextWidget $ ffor doneE $ const $ Retractable{
        retractableNext = settingsPage
      , retractablePrev = thisWidget
      }
