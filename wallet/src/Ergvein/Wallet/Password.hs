{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Password(
    setupPassword
  , submitSetBtn
  , setupLoginPassword
  , askTextPassword
  , askPassword
  , askPasswordModal
  , setupLogin
  , setupPattern
  , setupDerivPrefix
  ) where

import Control.Monad.Except
import Data.Either (fromRight)
import Data.Maybe
import Data.Time (getCurrentTime)
import Reflex.Localize.Dom

import Ergvein.Types
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Localization.PatternKey
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Util
import Sepulcas.Elements
import Sepulcas.Elements.Input
import Sepulcas.Monad
import Sepulcas.Native
import Sepulcas.Platform
import Sepulcas.Text
import Sepulcas.Validate

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

submitSetBtn :: MonadFrontBase t m => m (Event t ())
submitSetBtn = submitClass "button button-outline" PWSSet

setupPassword :: MonadFrontBase t m => Event t () -> m (Event t Password)
setupPassword e = divClass "setup-password" $ form $ fieldset $ mdo
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    check PWSNoMatch $ p1 == p2
    pure p1

setupLoginPassword :: MonadFrontBase t m => Maybe Text -> Event t () -> m (Event t (Text, Password))
setupLoginPassword mlogin e = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textFieldAttr PWSLogin ("placeholder" =: "my wallet name") $ fromMaybe "" mlogin
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    l  <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    check PWSNoMatch $ p1 == p2
    pure (l,p1)

passwordHeader :: MonadFrontBase t m => m (Event t ())
passwordHeader =
  divClass "header-wrapper mb-1" $
    divClass "header header-black" $
      divButton "header-button ml-a" $
        elClass "i" "fas fa-times fa-fw" $ pure ()

setupPattern :: MonadFrontBase t m => m (Event t Password)
setupPattern = divClass "setup-password" $ form $ fieldset $ mdo
  pD <- patternSaveWidget
  pE <- delay 0.1 $ updated pD
  validate $ poke pE $ const $ runExceptT $ do
    p <- sampleDyn pD
    check PWSEmptyPattern $ not $ T.null p
    pure p

setupLogin :: MonadFrontBase t m => Event t () -> m (Event t Text)
setupLogin e = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textField PWSLogin ""
  validate $ poke e $ const $ runExceptT $ do
    l <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    pure l

setupDerivPrefix :: MonadFrontBase t m => [Currency] -> Maybe DerivPrefix -> m (Dynamic t DerivPrefix)
setupDerivPrefix ac mpath = do
  divClass "password-setup-descr" $ h5 $ localizedText PWSDerivDescr
  divClass "setup-password" $ form $ fieldset $ mdo
    let dval = fromMaybe defValue mpath
    pathTD <- textField PWSDeriv $ showDerivPath dval
    pathE <- validate $ ffor (updated pathTD) $ maybe (Left PWSInvalidPath) Right . parseDerivePath
    holdDyn dval pathE
  where
    defValue = case ac of
      [] -> defaultDerivePath BTC
      [c] -> defaultDerivePath c
      _ -> defaultDerivPathPrefix

askPassword :: MonadFrontBase t m => Text -> Bool -> m (Event t Password)
askPassword name writeMeta
  | isAndroid = askPasswordAndroid name writeMeta
  | otherwise = askTextPassword PPSUnlock (PWSPassNamed name)

askTextPassword :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPassword title description = do
  divClass "password-ask-title" $ h4 $ localizedText title
  divClass "ask-password" $ form $ fieldset $ do
    pD <- passFieldWithEye description
    e <- submitClass "button button-outline" PWSGo
    pure $ tag (current pD) e

askPasswordAndroid :: MonadFrontBase t m => Text -> Bool -> m (Event t Password)
askPasswordAndroid name writeMeta = mdo
  let fpath = "meta_wallet_" <> (T.replace " " "_" name)
  isPass0 <- fmap (fromRight False) $ retrieveValue fpath False
  isPassD <- holdDyn isPass0 tglE
  valD <- networkHoldDyn $ ffor isPassD $ \isPass -> if isPass
    then askPasswordImpl name writeMeta
    else askPatternImpl name writeMeta
  let (passE, tglE) = (\(a,b) -> (switchDyn a, switchDyn b)) $ splitDynPure valD
  pure passE

askPasswordImpl :: MonadFrontBase t m => Text -> Bool -> m (Event t Password, Event t Bool)
askPasswordImpl name writeMeta = do
  let fpath = "meta_wallet_" <> (T.replace " " "_" name)
  when writeMeta $ storeValue fpath True True
  divClass "password-ask-title" $ h4 $ localizedText PPSUnlock
  divClass "ask-password" $ form $ fieldset $ do
    pD <- passFieldWithEye $ PWSPassNamed name
    divClass "fit-content ml-a mr-a" $ do
      e <- divClass "" $ submitClass "button button-outline w-100" PWSGo
      patE <- divClass "" $ submitClass "button button-outline w-100" PatPSUsePattern
      pure $ (tag (current pD) e, False <$ patE)

askPatternImpl :: MonadFrontBase t m => Text -> Bool -> m (Event t Password, Event t Bool)
askPatternImpl name writeMeta = do
  let fpath = "meta_wallet_" <> (T.replace " " "_" name)
  when writeMeta $ storeValue fpath False True
  divClass "password-ask-title" $ h5 $ localizedText PKSUnlock
  divClass "password-ask-title" $ h5 $ localizedText $ PKSFor name
  patE <- divClass "ask-pattern" $ form $ fieldset $ mdo
    c <- loadCounter
    let cInt = case (Map.lookup name (patterntriesCount c)) of
          Just p -> p
          Nothing -> 0
    now <- liftIO $ getCurrentTime
    a <- (clockLossy 1 now)
    freezeD <- networkHold (pure False) $ ffor (updated a) $ \TickInfo{..} -> do
      cS <- sampleDyn counterD
      let cdTime = if cS < 5
            then 0
            else 30 * (2 ^ (cS - 5))
      if (cdTime - _tickInfo_n) > 0
      then do
        divClass "backcounter" $ text $ "You should wait " <> (showt $ cdTime - _tickInfo_n) <> " sec"
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

askPasswordModal :: MonadFrontBase t m => m ()
askPasswordModal
  | isAndroid = askPasswordModalAndroid
  | otherwise = askPasswordModalDesc

askPasswordModalAndroid :: MonadFrontBase t m => m ()
askPasswordModalAndroid = mdo
  goE  <- fmap fst getPasswordModalEF
  fire <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE, Nothing <$ closeE]
  valD <- networkHold (pure (never, never)) $ ffor redrawE $ \case
    Just (i, name) -> divClass "ask-pattern-modal" $ do
      closeE' <- passwordHeader
      passE' <- divClass "mt-1 ml-1 mr-1" $ do
        askPassword name False
      pure (fmap ((i,) . Just) passE', closeE')
    Nothing -> pure (never, never)
  let (passD, closeD) = splitDynPure valD
  let passE = switchDyn passD
  let closeE = switchDyn closeD
  performEvent_ $ (liftIO . fire) <$> passE

askPasswordModalDesc :: MonadFrontBase t m => m ()
askPasswordModalDesc = mdo
  goE  <- fmap fst getPasswordModalEF
  fire <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE, Nothing <$ closeE]
  valD <- networkHold (pure (never, never)) $ ffor redrawE $ \case
    Just (i, name) -> divClass "ask-password-modal" $ do
      closeE' <- passwordHeader
      passE' <- (fmap . fmap) ((i,) . Just) $ divClass "ask-password-modal-content" $ askPassword name False
      pure (passE', closeE')
    Nothing -> pure (never, never)
  let (passD, closeD) = splitDynPure valD
  let passE = switchDyn passD
  let closeE = switchDyn closeD
  performEvent_ $ (liftIO . fire) <$> passE
