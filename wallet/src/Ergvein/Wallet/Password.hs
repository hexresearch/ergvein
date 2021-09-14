{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Password(
    setupPassword
  , submitSetBtn
  , setupLoginPassword
  , askTextPasswordWidget
  , askPasswordWidget
  , askPasswordModal
  , setupLogin
  , setupPattern
  , setupDerivPrefix
  , nameProposal
  , check
  ) where

import Control.Monad.Except
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.List
import Data.Maybe
import Data.Time (getCurrentTime)
import Reflex.Localize.Dom

import Ergvein.Wallet.Localize
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Page.PinCode
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements
import Sepulcas.Validate

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as S

-- | Helper to throw error when predicate is not 'True'
check :: MonadError a m => a -> Bool -> m ()
check a False = throwError a
check _ True = pure ()

submitSetBtn :: MonadFrontBase t m => m (Event t ())
submitSetBtn = submitClass "button button-outline" PWSSet

setupPassword :: MonadFrontBase t m => Event t () -> m (Event t Password)
setupPassword e = divClass "setup-password" $ form $ fieldset $ mdo
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  validateEvent $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    check PWSNoMatch $ p1 == p2
    pure p1

setupLoginPassword :: MonadFrontBase t m => Maybe Text -> Event t () -> m (Event t (Text, Password))
setupLoginPassword mlogin e = divClass "setup-password" $ form $ fieldset $ mdo
  existingWalletNames <- listStorages
  loginD <- textFieldAttr PWSLogin ("placeholder" =: "my wallet name") $ fromMaybe (nameProposal existingWalletNames) mlogin
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  validateEvent $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    l  <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    check PWSNoMatch $ p1 == p2
    pure (l,p1)

nameProposal :: [WalletName] -> WalletName
nameProposal s = let
  ss = S.fromList s
  in fromJust $ find (not . flip S.member ss) $ firstName : ((subsequentNamePrefix <>) . showt <$> [2 ::Int ..])
  where
   firstName = "main"
   subsequentNamePrefix = "wallet_"

passwordHeader :: MonadFrontBase t m => m (Event t ())
passwordHeader =
  divClass "header-wrapper mb-1" $
    divClass "header header-black" $
      divButton "header-button header-button-left" $
        elClass "i" "fas fa-chevron-left" $ pure ()

setupPattern :: MonadFrontBase t m => m (Event t Password)
setupPattern = divClass "setup-password" $ form $ fieldset $ mdo
  pD <- patternSaveWidget
  pE <- delay 0.1 $ updated pD
  validateEvent $ poke pE $ const $ runExceptT $ do
    p <- sampleDyn pD
    check PWSEmptyPattern $ not $ T.null p
    pure p

setupLogin :: MonadFrontBase t m => Event t () -> m (Event t Text)
setupLogin e = divClass "setup-password" $ form $ fieldset $ mdo
  existingWalletNames <- listStorages
  loginD <- textField PWSLogin (nameProposal existingWalletNames)
  validateEvent $ poke e $ const $ runExceptT $ do
    l <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    pure l

setupDerivPrefix :: MonadFrontBase t m => [Currency] -> Maybe DerivPrefix -> m (Dynamic t DerivPrefix)
setupDerivPrefix ac mpath = do
  divClass "password-setup-descr" $ h5 $ do
    localizedText PWSDerivDescr1
    br
    localizedText PWSDerivDescr2
  divClass "setup-password" $ form $ fieldset $ mdo
    let dval = fromMaybe defValue mpath
    pathTD <- textField PWSDeriv $ showDerivPath dval
    pathE <- validateEvent $ ffor (updated pathTD) $ maybe (Left PWSInvalidPath) Right . parseDerivePath
    holdDyn dval pathE
  where
    defValue = case ac of
      [] -> defaultDerivePath BTC
      [c] -> defaultDerivePath c
      _ -> defaultDerivPathPrefix

askPasswordWidget :: MonadFrontBase t m => Text -> Bool -> m (Event t Password)
askPasswordWidget name writeMeta
  | isAndroid = askPasswordAndroidWidget name writeMeta
  | otherwise = askTextPasswordWidget PPSUnlock (PWSPassNamed name)

askTextPasswordWidget :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPasswordWidget title description = divClass "my-a" $ do
  h4 $ localizedText title
  divClass "" $ do
    pD <- passFieldWithEye description
    e <- submitClass "button button-outline" PWSGo
    pure $ tag (current pD) e

askPasswordAndroidWidget :: MonadFrontBase t m => Text -> Bool -> m (Event t Password)
askPasswordAndroidWidget name writeMeta = mdo
  let fpath = "meta_wallet_" <> T.replace " " "_" name
  isPass <- fromRight False <$> retrieveValue fpath False
  passE <- if isPass
    then askPasswordImpl name writeMeta
    else askPinCodeImpl name writeMeta
  pure passE

askPasswordImpl :: MonadFrontBase t m => Text -> Bool -> m (Event t Password)
askPasswordImpl name writeMeta = do
  let fpath = "meta_wallet_" <> T.replace " " "_" name
  when writeMeta $ storeValue fpath True True
  divClass "ask-password my-a" $ form $ fieldset $ do
    h4 $ localizedText PPSUnlock
    pD <- passFieldWithEye $ PWSPassNamed name
    divClass "fit-content ml-a mr-a" $ do
      e <- divClass "" $ submitClass "button button-outline w-100" PWSGo
      pure $ tag (current pD) e

askPinCodeImpl :: MonadFrontBase t m => Text -> Bool -> m (Event t Password)
askPinCodeImpl name writeMeta = do
  let fpath = "meta_wallet_" <> T.replace " " "_" name
  when writeMeta $ storeValue fpath False True
  pinE <- mdo
    c <- loadCounter
    let cInt = fromMaybe 0 $ Map.lookup name (patterntriesCount c)
    now <- liftIO getCurrentTime
    a <- clockLossy 1 now
    freezeD <- networkHold (pure False) $ ffor (updated a) $ \TickInfo{..} -> do
      cS <- sampleDyn counterD
      let cdTime = if cS < 5
            then 0
            else 30 * (2 ^ (cS - 5))
      if (cdTime - _tickInfo_n) > 0
      then do
        divClass "backcounter" $ text $ "You should wait " <> showt (cdTime - _tickInfo_n) <> " sec"
        pure True
      else
        pure False
    passE <- pinCodeAskWidget PinCodePSEnterPinCode
    counterD <- holdDyn cInt $ poke passE $ \_ -> do
      freezeS <- sampleDyn freezeD
      cS <- sampleDyn counterD
      if freezeS
        then pure cS
        else pure $ cS + 1
    performEvent_ $ ffor (updated counterD) $ \cS ->
      saveCounter $ PatternTries $ Map.insert name cS (patterntriesCount c)
    pure $ attachPromptlyDynWithMaybe (\freeze p -> if not freeze then Just p else Nothing) freezeD passE
  pure pinE

askPatternImpl :: MonadFrontBase t m => Text -> Bool -> m (Event t Password, Event t Bool)
askPatternImpl name writeMeta = do
  let fpath = "meta_wallet_" <> T.replace " " "_" name
  when writeMeta $ storeValue fpath False True
  h5 $ localizedText PKSUnlock
  h5 $ localizedText $ PKSFor name
  patE <- divClass "ask-pattern" $ form $ fieldset $ mdo
    c <- loadCounter
    let cInt = fromMaybe 0 $ Map.lookup name (patterntriesCount c)
    now <- liftIO getCurrentTime
    a <- clockLossy 1 now
    freezeD <- networkHold (pure False) $ ffor (updated a) $ \TickInfo{..} -> do
      cS <- sampleDyn counterD
      let cdTime = if cS < 5
            then 0
            else 30 * (2 ^ (cS - 5))
      if (cdTime - _tickInfo_n) > 0
      then do
        divClass "backcounter" $ text $ "You should wait " <> showt (cdTime - _tickInfo_n) <> " sec"
        pure True
      else
        pure False
    pD <- patternAskWidget
    counterD <- holdDyn cInt $ poke (updated pD) $ \_ -> do
      freezeS <- sampleDyn freezeD
      cS <- sampleDyn counterD
      if freezeS
        then pure cS
        else pure $ cS + 1
    performEvent_ $ ffor (updated counterD) $ \cS ->
      saveCounter $ PatternTries $ Map.insert name cS (patterntriesCount c)
    pure $ attachPromptlyDynWithMaybe (\freeze p -> if not freeze then Just p else Nothing) freezeD (updated pD)
  passE <- submitClass "button button-outline" PatPSUsePass
  pure (patE, True <$ passE)

askPasswordModal :: MonadFront t m => m ()
askPasswordModal = mdo
  goE <- fmap fst getPasswordModalEF
  fire <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE, Nothing <$ closeE]
  valD <- networkHold (pure (never, never)) $ ffor redrawE $ \case
    Just (i, name) -> divClass "ask-password-modal" $ do
      title <- localized PWSPassword
      (closeE', passE') <- wrapperPasswordModal title "password-widget-container" $
        fmap ((i,) . Just) <$> askPasswordWidget name False
      pure (passE', closeE')
    Nothing -> pure (never, never)
  let (passD, closeD) = splitDynPure valD
      passE = switchDyn passD
      closeE = switchDyn closeD
  performEvent_ $ liftIO . fire <$> passE
