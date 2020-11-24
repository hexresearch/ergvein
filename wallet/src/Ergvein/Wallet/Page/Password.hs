{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  , askTextPasswordPage
  , changePasswordWidget
  ) where

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Restore
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Password
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Wrapper
import Reflex.ExternalRef
import Reflex.Localize

import Ergvein.Wallet.Language

import qualified Data.Text as T

passwordPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> [Currency] -> Maybe Text -> m ()
passwordPage wt mpath mnemonic curs mlogin = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    logPassE <- setupLoginPassword mlogin btnE
    pathD <- setupDerivPrefix curs mpath
    btnE <- submitSetBtn
  let goE = current pathD `attach` logPassE
  void $ nextWidget $ ffor goE $ \(path, (login,pass)) -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt mnemonic curs login pass (Just path) True
        else performAuth wt mnemonic curs login pass (Just path) True
    , retractablePrev = if pass == ""
        then Just $ pure $ passwordPage wt (Just path) mnemonic curs (Just login)
        else Nothing
    }

confirmEmptyPage :: MonadFrontBase t m
  => WalletSource
  -> Mnemonic
  -> [Currency]
  -> Text
  -> Password
  -> Maybe DerivPrefix
  -> Bool
  -> m ()
confirmEmptyPage wt mnemonic curs login pass mpath isPass = wrapperSimple True $ do
  h4 $ localizedText CEPAttention
  h5 $ localizedText CEPConsequences
  divClass "fit-content ml-a mr-a" $ do
    setE <- divClass "" (submitClass "button button-outline w-100" PWSSet)
    retract =<< divClass "" (submitClass "button button-outline w-100" CEPBack)
    void $ nextWidget $ ffor setE $ const $ Retractable {
        retractableNext = performAuth wt mnemonic curs login pass mpath isPass
      , retractablePrev = Nothing
      }

performAuth :: MonadFrontBase t m
  => WalletSource
  -> Mnemonic
  -> [Currency]
  -> Text
  -> Password
  -> Maybe DerivPrefix
  -> Bool
  -> m ()
performAuth wt mnemonic curs login pass mpath isPass = do
  buildE <- getPostBuild
  storage <- initAuthInfo wt mpath mnemonic curs login pass isPass
  authInfoE <- handleDangerMsg $ storage <$ buildE
  void $ setAuthInfo $ Just <$> authInfoE

setupLoginPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> [Currency] -> m ()
setupLoginPage wt mpath mnemonic curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  rec
    logD <- holdDyn "" =<< setupLogin btnE
    pathD <- setupDerivPrefix curs mpath
    btnE <- submitSetBtn
  void $ nextWidget $ ffor (updated ((,) <$> logD <*> pathD)) $ \(l, path) -> Retractable {
      retractableNext = setupPatternPage wt (Just path) mnemonic l curs
    , retractablePrev = Just $ pure $ setupLoginPage wt (Just path) mnemonic curs
    }

setupPatternPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> Text -> [Currency] -> m ()
setupPatternPage wt mpath mnemonic l curs = wrapperSimple True $ do
  let this = Just $ pure $ setupPatternPage wt mpath mnemonic l curs
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
        then confirmEmptyPage wt mnemonic curs l pass mpath False
        else performAuth wt mnemonic curs l pass mpath False
    , retractablePrev = if pass == "" then this else Nothing
    }
  void $ nextWidget $ ffor setPassE $ const $ Retractable {
      retractableNext = setupMobilePasswordPage wt mpath mnemonic l curs
    , retractablePrev = this
    }

setupMobilePasswordPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> Text -> [Currency] -> m ()
setupMobilePasswordPage wt mpath mnemonic l curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSPassTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    passE <- setupPassword btnE
    btnE <- divClass "fit-content ml-a mr-a" $ do
      btnE <- divClass "" $ (submitClass "button button-outline w-100" PWSSet)
      setPattE <- divClass "" $ submitClass "button button-outline w-100" PatPSPatt
      retract setPattE
      pure btnE
  void $ nextWidget $ ffor passE $ \pass -> Retractable {
      retractableNext = if pass == ""
        then confirmEmptyPage wt mnemonic curs l pass mpath True
        else performAuth wt mnemonic curs l pass mpath True
    , retractablePrev = if pass == ""
        then Just $ pure $ setupMobilePasswordPage wt mpath mnemonic l curs
        else Nothing
    }

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapperSimple True $ askPassword name True

askTextPasswordPage :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPasswordPage title description = wrapperSimple True $ askTextPassword title description

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
#ifdef ANDROID
changePasswordWidget = changePasswordMobileWidget
#else
changePasswordWidget = changePasswordDescWidget
#endif

changePasswordDescWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordDescWidget = wrapperSimple True $ mdo
  tglD <- holdDyn False tglE
  (passE, tglE) <- fmap switchDyn2 $ widgetHoldDyn $ ffor tglD $ \case
    True  -> (fmap . fmap) (False <$) confirmEmptyWidget
    False -> mdo
      changePasswordDescr True
      passE' <- setupPassword btnE
      btnE <- submitSetBtn
      let (emptyE, passE) = splitFilter (== "") passE'
      pure (passE, True <$ emptyE)
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

data CPMStage = CPMPattern | CPMPassword | CPMEmpty Bool

changePasswordMobileWidget :: MonadFront t m => m (Event t (Password, Bool))
changePasswordMobileWidget = wrapperSimple True $ mdo
  login <- fmap (_authInfo'login) $ readExternalRef =<< getAuthInfoRef
  let name = T.replace " " "_" login
  stage0 <- fmap eitherToStage $ retrieveValue ("meta_wallet_" <> name) False
  stageD <- holdDyn stage0 nextE
  (patE, nextE) <- fmap switchDyn2 $ widgetHoldDyn $ ffor stageD $ \case
    CPMEmpty b -> do
      (passE, backE) <- confirmEmptyWidget
      pure ((,b) <$> passE, boolToStage b <$ backE)
    CPMPassword -> mdo
      changePasswordDescr True
      passE' <- setupPassword btnE
      (btnE, setPattE) <- divClass "fit-content ml-a mr-a" $ do
        btnE <- divClass "" $ (submitClass "button button-outline w-100" PWSSet)
        setPattE <- divClass "" $ submitClass "button button-outline w-100" PatPSPatt
        pure (btnE, setPattE)
      let (emptyE, passE) = splitFilter (== "") passE'
      let nextE = leftmost [CPMEmpty True <$ emptyE, CPMPattern <$ setPattE]
      pure ((, True) <$> passE, nextE)
    CPMPattern -> do
      changePasswordDescr False
      patE <- setupPattern
      divClass "fit-content ml-a mr-a" $ do
        setPassE <- divClass "" $ submitClass "button button-outline w-100" PatPSPass
        skipE <- divClass "" $ submitClass "button button-outline w-100" CEPSkip
        let nextE = leftmost [CPMPassword <$ setPassE, CPMEmpty False <$ skipE]
        pure ((, False) <$> patE, nextE)
  pure patE
  where
    boolToStage b = if b then CPMPassword else CPMPattern
    eitherToStage = either (const CPMPattern) boolToStage
