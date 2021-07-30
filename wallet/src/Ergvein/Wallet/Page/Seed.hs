{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    backupPage
  , mnemonicPage
  , setLoginPasswordPage
  , mnemonicWidget
  , simpleSeedRestorePage
  , seedRestorePage
  ) where

import Control.Monad.Random.Strict
import Data.Bifunctor
import Data.List (permutations, (\\))
import Data.Maybe
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (for)
import Reflex.Localize.Dom
import System.Random.Shuffle

import {-# SOURCE #-} Ergvein.Wallet.Page.History.Btc
import Ergvein.Crypto
import Ergvein.Either
import Ergvein.Text
import Ergvein.Types.Restore
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Camera
import Sepulcas.Clipboard
import Sepulcas.Elements
import Sepulcas.Resize

import qualified Data.List      as L
import qualified Data.Serialize as S
import qualified Data.Text      as T
import qualified Data.Vector    as V

data GoPage = GoBackupNow | GoBackupLater

backupPage :: MonadFrontBase t m => m ()
backupPage = wrapperSimple True $ do
  divClass "backup-page-icon mb-2" $ elClass "i" "fas fa-exclamation-triangle" blank
  h4 $ localizedText SPSBackupTitle
  parClass "ta-l" $ localizedText SPSBackupText1
  parClass "ta-l" $ localizedText SPSBackupText2
  btnE <- divClass "initial-options grid1" $ do
    backupNowBtnE <- (GoBackupNow <$) <$> outlineButton SPSBackupNow
    backupLaterBtnE <- (GoBackupLater <$) <$> buttonClass "button button-clear" SPSBackupLater
    pure $ leftmost [backupNowBtnE, backupLaterBtnE]
  goE <- performFork $ ffor btnE $ \act -> do
    entropy <- liftIO getEntropy
    pure $ fmap (act,) <$> first T.pack $ toMnemonic entropy
  goE' <- handleDangerMsg goE
  void $ nextWidget $ ffor goE' $ \(act, mnemonic) -> Retractable {
      retractableNext = case act of
        GoBackupNow -> mnemonicPageUnauth $ Just mnemonic
        GoBackupLater -> setLoginPasswordPage WalletGenerated True mnemonic
    , retractablePrev = Just $ pure backupPage
    }

mnemonicPageUnauth :: MonadFrontBase t m => Maybe Mnemonic -> m ()
mnemonicPageUnauth mMnemonic = wrapperSimple True $ do
  (e, mnemonicD) <- mnemonicWidget mMnemonic
  void $ nextWidget $ ffor e $ \mn -> Retractable {
      retractableNext = checkPageUnauth mn
    , retractablePrev = Just $ mnemonicPageUnauth <$> mnemonicD
    }

mnemonicPage :: MonadFront t m => Maybe Mnemonic -> m ()
mnemonicPage mMnemonic = wrapperSimple True $ do
  (e, mnemonicD) <- mnemonicWidget mMnemonic
  void $ nextWidget $ ffor e $ \mn -> Retractable {
      retractableNext = checkPage mn
    , retractablePrev = Just $ mnemonicPage <$> mnemonicD
    }

setLoginPasswordPage :: MonadFrontBase t m => WalletSource -> Bool -> Mnemonic -> m ()
setLoginPasswordPage walletSource seedBackupRequired mnemonic = if isAndroid
  then setupLoginPage walletSource seedBackupRequired Nothing mnemonic activeCurrencies
  else setupPasswordPage walletSource seedBackupRequired Nothing mnemonic activeCurrencies Nothing
  where activeCurrencies = [BTC]

checkPageUnauth :: MonadFrontBase t m => Mnemonic -> m ()
checkPageUnauth mnemonic = wrapperSimple True $ do
  mnemonicE <- mnemonicCheckWidget mnemonic
  void $ nextWidget $ ffor mnemonicE $ \mnemonic' -> Retractable {
      retractableNext = setLoginPasswordPage WalletGenerated False mnemonic'
    , retractablePrev = Just $ pure $ checkPageUnauth mnemonic'
    }

checkPage :: MonadFront t m => Mnemonic -> m ()
checkPage mnemonic = wrapperSimple True $ do
  mnemonicE <- mnemonicCheckWidget mnemonic
  _ <- setSeedBackupRequired $ False <$ mnemonicE
  void $ nextWidget $ ffor mnemonicE $ \mnemonic' -> Retractable {
      retractableNext = historyPage
    , retractablePrev = Just $ pure $ checkPage mnemonic'
    }

generateMnemonic :: MonadFrontBase t m => m (Maybe Mnemonic)
generateMnemonic = do
  e <- liftIO getEntropy
  validateNow $ first T.pack $ toMnemonic e

-- | Generate and show mnemonic phrase to user. Returned dynamic is state of widget.
mnemonicWidget :: MonadFrontBase t m => Maybe Mnemonic -> m (Event t Mnemonic, Dynamic t (Maybe Mnemonic))
mnemonicWidget mnemonic = do
  mphrase <- maybe generateMnemonic (pure . Just) mnemonic
  case mphrase of
    Nothing -> pure (never, pure Nothing)
    Just phrase -> mdo
      divClass "mnemonic-title" $ h4 $ localizedText SPSTitle
      void $ divClass "mnemonic-colony" $ adaptive3 (smallMnemonic phrase) (mediumMnemonic phrase) (desktopMnemonic phrase)
      divClass "mnemonic-warn" $ h4 $ localizedText SPSWarn
      btnE <- outlineButton SPSWrote
      pure (phrase <$ btnE, pure $ Just phrase)
  where
    prepareMnemonic :: Int -> Mnemonic -> [(Int, Text)]
    prepareMnemonic cols = L.concat . L.transpose . mkCols cols . zip [1..] . T.words

    wordColumn cs i w = divClass ("column " <> cs) $ do
      elClass "span" "text-muted" $ text $ showt i <> " "
      text w

    smallMnemonic phrase = for_ (zip [(1 :: Int)..] . T.words $ phrase) $ uncurry (wordColumn "mnemonic-word-mb")
    mediumMnemonic phrase =  void $ colonize 2 (prepareMnemonic 2 phrase) $ uncurry (wordColumn "mnemonic-word-md")
    desktopMnemonic phrase = void $ colonize 4 (prepareMnemonic 4 phrase) $ uncurry (wordColumn "mnemonic-word-dx")

-- | Helper to cut a list into column-length chunks
mkCols :: Int -> [a] -> [[a]]
mkCols n vals = mkCols' [] vals
  where
    l = length vals
    n' = l `div` n + if l `mod` n /= 0 then 1 else 0 -- n - is the number of columns with n' elems in eacn
    mkCols' :: [[a]] -> [a] -> [[a]]
    mkCols' acc xs = case xs of
      [] -> acc
      _ -> let (r, rest) = L.splitAt n' xs in mkCols' (acc ++ [r]) rest

-- | Creates button with number and text. Number has muted color.
numberedButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> Int -> lbl -> m (Event t ())
numberedButton classValD number lbl = mkButton "button" [("onclick", "return false;")] classValD $ do
  spanClass "text-muted" $ text $ showt number <> " "
  localizedText lbl

-- | Creates button that can be disabled or enabled by dynamic.
switchableButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Text -> Dynamic t Bool -> Text -> lbl -> m (Event t ())
switchableButton classVal isDisabledD disabledClass lbl =
  let attrsD = ffor isDisabledD $ \isDisabled ->
        let classes = if isDisabled && not (T.null disabledClass)
              then classVal <> " " <> disabledClass
              else classVal
            attrs = if isDisabled
             then [("onclick", "return false;"), ("class", classes), ("disabled", "disabled")]
             else [("onclick", "return false;"), ("class", classes)]
        in attrs
  in mkButtonDynAttr "button" attrsD $ localizedText lbl

-- | Interactive check of mnemonic phrase
-- Takes correct mnemonic as a parameter
-- Returns an event with the correct mnemonic when the user has successfully passed the verification process
mnemonicCheckWidget :: MonadFrontBase t m => Mnemonic -> m (Event t Mnemonic)
mnemonicCheckWidget mnemonic = mdo
  let ws = T.words mnemonic
      indexedWords = zip [0..] ws -- We need to deal with indices because there might be repetitions in mnemonic
  randGen <- liftIO newStdGen
  let shuffledWords = shuffle' indexedWords (length indexedWords) randGen
  langD <- getLanguage
  h4 $ localizedText SPSVerifyTitle
  h5 $ spanClass "text-muted" $ localizedText SPSVerifyDescr
  selectedWordsD <- foldDyn (\(append, x) xs -> if append then xs <> [x] else L.delete x xs) [] $ leftmost [wordSelectedE, wordUnselectedE] -- Contains a list of selected words
  let isCorrectOrderD = ffor selectedWordsD (\selectedWords -> (snd <$> selectedWords) `L.isPrefixOf` ws) -- Contains True if the order of selected words is correct, False otherwise
  wordUnselectedE <- divClass "mnemonic-verification-container mb-2" $ do
    unselectedE <- divClass "mnemonic-verification-btn-container" $ do
      pressedEventsD <- networkHoldDyn $ ffor selectedWordsD $ \words -> do
        unselectBtnEvents <- for (zip [1..] words) $ \(i, iw@(index, word)) -> do
          pressedE <- numberedButton "button button-outline" i word
          pure $ (False, iw) <$ pressedE -- False means that we unselected this word and we need to remove it from selectedWordsD
        pure $ leftmost unselectBtnEvents
      pure $ switchDyn pressedEventsD
    void $ networkHoldDyn $ ffor isCorrectOrderD $ \isCorrectOrder -> do
      if isCorrectOrder
        then
          pure ()
        else
          spanClass "mnemonic-verification-error" $ do
            elClass "i" "fas fa-exclamation-circle" blank
            text " "
            localizedText SPSVerifyError
    pure unselectedE
  wordSelectedE <- divClass "mnemonic-verification-btn-container mb-2" $ do
    selectBtnEvents <- for shuffledWords $ \iw@(index, word) -> do
      pressedE <- switchableButton "button button-outline" ((iw `elem`) <$> selectedWordsD) "mnemonic-word-disabled" word
      pure $ (True, iw) <$ pressedE -- Ture means that we selected this word and we need to append it to selectedWordsD
    pure $ leftmost selectBtnEvents
  let okD = ffor (zipDynWith (,) selectedWordsD isCorrectOrderD) $ \(selectedWords, isCorrectOrder) ->
        (length selectedWords == length ws) && isCorrectOrder
  submitE <- switchableButton "button button-outline" (not <$> okD) "" CSForward
  let goE = tagPromptlyDyn okD submitE
  pure $ fforMaybe goE $ \ok -> if ok then Just mnemonic else Nothing

pasteBtn :: MonadFrontBase t m => m (Event t ())
pasteBtn = outlineTextIconButtonTypeButton CSPaste "fas fa-clipboard fa-lg"

scanQRBtn :: MonadFrontBase t m => m (Event t ())
scanQRBtn = outlineTextIconButtonTypeButton CSScanQR "fas fa-qrcode fa-lg"

askSeedPasswordPage :: MonadFrontBase t m => EncryptedByteString -> m ()
askSeedPasswordPage encryptedMnemonic = do
  passE <- askTextPasswordPage PPSMnemonicUnlock ("" :: Text)
  let mnemonicBSE = decryptBSWithAEAD encryptedMnemonic <$> passE
  verifiedMnemonicE <- handleDangerMsg mnemonicBSE
  void $ nextWidget $ ffor (decodeUtf8With lenientDecode <$> verifiedMnemonicE) $ \mnem -> Retractable {
      retractableNext = setLoginPasswordPage WalletRestored False mnem
    , retractablePrev = Just $ pure $ askSeedPasswordPage encryptedMnemonic
    }

simpleSeedRestorePage :: MonadFrontBase t m => m ()
simpleSeedRestorePage = plainRestorePage 12

seedRestorePage :: MonadFrontBase t m => m ()
seedRestorePage = wrapperSimple True $ do
  h2 $ localizedText SPSTypeTitle
  let cls = "ml-a mr-a mb-2 w-80" <> if isAndroid then " fit-content" else " navbar-2-cols"
      btnCls = "button button-outline" <>
        if isAndroid then " disp-block w-100" else ""
  goE <- divClass cls $ do
    e1 <-(True <$) <$> buttonClass btnCls SPSPlain
    e2 <- (False <$) <$> buttonClass btnCls SPSBase58
    pure $ leftmost [e1, e2]
  void $ nextWidget $ ffor goE $ \b -> Retractable {
      retractableNext = if b
        then lengthSelectPage
        else base58RestorePage
    , retractablePrev = Just $ pure seedRestorePage
    }

lengthSelectPage :: MonadFrontBase t m => m ()
lengthSelectPage = wrapperSimple True $ do
  h2 $ localizedText SPSLengthTitle
  let cls = "ml-a mr-a mb-2 w-80" <> if isAndroid then "" else " navbar-5-cols"
  goE <- divClass cls $ mdo
    leftmost <$> traverse mkBtn [24,21,18,15,12]
  void $ nextWidget $ ffor goE $ \i -> Retractable {
      retractableNext = plainRestorePage i
    , retractablePrev = Just $ pure lengthSelectPage
    }
  where
    btnCls = "button button-outline" <> if isAndroid then " mlr-a disp-block" else ""
    mkBtn i = fmap (i <$) $ buttonClass btnCls $ showt i

pasteBtnsWidget :: MonadFrontBase t m => m (Event t Text)
pasteBtnsWidget = divClass "restore-seed-buttons-wrapper" $ do
  pasteBtnE <- pasteBtn
  pasteE <- clipboardPaste pasteBtnE
  if isAndroid
    then do
      qrCodeBtnE <- scanQRBtn
      openCameraE <- delay 1.0 =<< openCamara qrCodeBtnE
      resQRCodeE <- waiterResultCamera openCameraE
      pure $ leftmost [pasteE, resQRCodeE]
    else pure pasteE

data ParseState
  = PSWaiting
  | PSDone Text
  | PSSuggs [Text] [Text]
  | PSWordError Text
  | PSFullError [(Int, Text)]
  | PSExtraError Text

-- Parse the mnemonic phrase
-- If it's empty, return PSWaiting
-- If it's exactly `mnem` words, check if all words are correct, otherwise return error
-- If it's longer than `mnem` -- something went wrong, return error
-- If it's lesser that `mnem` words and the last symbol is " "
--   assume that the user wants to enter a new word and show PSWaiting again
-- Otherwise, assume it's a prefix, get all words from the wordlist with that prefix
-- If there is none, the prefix is misspelled, return error
-- Otherwise return suggestions along with the rest.
parseMnem :: Int -> Text -> ParseState
parseMnem l mnem = case ws of
  [] -> PSWaiting
  _ -> if length ws == l && wordTrieElem w
    then case filter (not . wordTrieElem . snd) iws of
      [] -> PSDone (T.intercalate " " ws)
      errs -> PSFullError errs
    else if length ws > l
      then PSExtraError mnem
      else if wordTrieElem w && T.takeEnd 1 mnem == " "
        then PSWaiting
        else case getWordsWithPrefix w of
          [] -> PSWordError mnem
          suggs -> PSSuggs (init ws) suggs
  where
    iws = zip [1..] ws
    ws = T.words $ T.toLower mnem
    w = last ws

plainRestorePage :: MonadFrontBase t m => Int -> m ()
plainRestorePage mnemLength = wrapperSimple True $ mdo
  h4 $ localizedText $ SPSPlainTitle mnemLength
  buildE <- getPostBuild

  let parsedD = parseMnem mnemLength <$> _textAreaElement_value ti
      enterKeyE = keypress Enter ti
      anyKeyE = domEvent Keypress ti
      -- resetE: overwrite all new writes if there was an unfixed error
      -- allows only del, backspace and arrows
      resetE = flip push anyKeyE $ const $ do
        state <- sampleDyn stateD
        pure $ case state of
          PSWordError t -> Just t
          PSExtraError t -> Just t
          _ -> Nothing

  fillE' <- delay 0.05 fillE    -- FRP network hangs without this delay
  stateD <- foldDyn (flip fromMaybe) PSWaiting $ leftmost [
      Nothing <$ fillE'
    , updated $ Just <$> parsedD
    , Just PSWaiting <$ buildE
    ]

  ti <- textAreaElement $ def & textAreaElementConfig_setValue .~ setValE
                              & (textAreaElementConfig_elementConfig . elementConfig_initialAttributes) .~
                                [("style", "resize:none"), ("class", "restore-seed-input")]

  let setValE = leftmost [pasteE, fillE, resetE]
  let tiEl = _element_raw $ _textAreaElement_element ti
  performEvent_ $ ffor setValE $ const $ selElementFocus tiEl
  fillE <- networkHoldE (pure never) $ ffor (updated stateD) $ \case
    PSWaiting         -> waiting
    PSWordError _     -> wordError
    PSDone _          -> doneText
    PSFullError errs  -> fullErr errs
    PSExtraError _    -> extraErr
    PSSuggs _ []      -> waiting
    PSSuggs ts ws -> case ws of
      [] -> wordError
      [w] -> divClass "restore-seed-buttons-wrapper" $ do
        let res = recombine ts w
        clickE <- buttonClass (pure "button button-outline") w
        pure $ res <$ leftmost [clickE, enterKeyE]
      _ -> do
        let ws' = take 6 ws
        wE <- divClass "restore-seed-buttons-wrapper" $ fmap leftmost $ for ws' $ \w -> do
          btnClickE <- buttonClass (pure "button button-outline") w
          pure $ w <$ btnClickE
        pure $ recombine ts <$> wE
  pasteE <- pasteBtnsWidget
  advancedE <- divClass "restore-seed-buttons-wrapper" $ outlineButton CSAdvanced
  void $ nextWidget $ ffor advancedE $ const $ Retractable {
      retractableNext = seedRestorePage
    , retractablePrev = Just $ pure simpleSeedRestorePage
  }
  void $ networkHold (pure ()) $ ffor (updated stateD ) $ \case
    PSDone mnem -> do
      submitE <- outlineButton CSForward
      void $ nextWidget $ ffor submitE $ const $ Retractable {
          retractableNext = setLoginPasswordPage WalletRestored False mnem
        , retractablePrev = Just $ pure seedRestorePage
        }
    _ -> pure ()

  pure ()
  where
    recombine ts w = T.intercalate " " (ts <> [w]) <> " "

    waiting   = h4 (localizedText SPSWaiting)     >> pure never
    wordError = h4 (localizedText SPSInvalidWord) >> pure never
    doneText  = h4 (localizedText SPSDone)        >> pure never
    extraErr  = h4 (localizedText SPSExtraWords)  >> pure never
    fullErr errs = do
      h4 $ localizedText SPSMisspelled
      void $ divClass "mb-1" $ for errs $
        divClass "" . localizedText . SPSMisspelledWord
      pure never

base58RestorePage :: MonadFrontBase t m => m ()
base58RestorePage = wrapperSimple True $ mdo
  h4 $ localizedText SPSBase58Title
  encodedEncryptedMnemonicErrsD <- holdDyn Nothing $ ffor validationE eitherToMaybe'
  encodedEncryptedMnemonicD <- validatedTextFieldSetValNoLabel "" encodedEncryptedMnemonicErrsD inputE
  inputE <- pasteBtnsWidget
  submitE <- networkHoldDynE $ ffor encodedEncryptedMnemonicD $ \v -> if v == ""
    then pure never
    else outlineButton CSForward
  let validationE = poke submitE $ \_ -> do
        encodedEncryptedMnemonic <- sampleDyn encodedEncryptedMnemonicD
        pure $ maybe (Left [SPSMnemonicDecodeError]) Right $
          (eitherToMaybe . S.decode <=< decodeBase58CheckBtc) encodedEncryptedMnemonic
      goE = fmapMaybe eitherToMaybe validationE
  void $ nextWidget $ ffor goE $ \encryptedMnemonic -> Retractable {
      retractableNext = askSeedPasswordPage encryptedMnemonic
    , retractablePrev = Just $ pure seedRestorePage
    }
