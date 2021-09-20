{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Password(
    PasswordTries(..)
  , saveCounter
  , loadCounter
  , setupPassword
  , submitSetBtn
  , setupLoginPassword
  , askTextPasswordWidget
  , askPasswordWidget
  , askPasswordModal
  , setupLogin
  , setupDerivPrefix
  , nameProposal
  , check
  ) where

import Control.Monad.Except
import Data.Either (fromRight)
import Data.List
import Data.Maybe
import Data.Time (getCurrentTime)
import Reflex.Localize.Dom

import Ergvein.Aeson
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.PinCode
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements
import Sepulcas.Validate

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T

data PasswordTries = PasswordTries {
  passwordTriesCount  :: Map.Map Text Integer
} deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "password") ''PasswordTries)

emptyPT :: PasswordTries
emptyPT = PasswordTries Map.empty
{-# INLINE emptyPT #-}

saveCounter :: (MonadIO m, PlatformNatives, HasStoreDir m) => PasswordTries -> m ()
saveCounter pt = storeValue "tries.json" pt True

loadCounter :: (MonadIO m, PlatformNatives, HasStoreDir m) => m PasswordTries
loadCounter = fmap (fromRight emptyPT) $ retrieveValue "tries" emptyPT

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
    let cInt = fromMaybe 0 $ Map.lookup name (passwordTriesCount c)
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
      saveCounter $ PasswordTries $ Map.insert name cS (passwordTriesCount c)
    pure $ attachPromptlyDynWithMaybe (\freeze p -> if not freeze then Just p else Nothing) freezeD passE
  pure pinE

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
