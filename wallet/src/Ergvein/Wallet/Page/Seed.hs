{-# LANGUAGE CPP #-}

-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicPage
  , mnemonicWidget
  , seedRestorePage
  , seedRestoreWidget
  ) where

import Control.Monad.Random.Strict
import Data.Bifunctor
import Data.Either (either)
import Data.List (permutations)
import Data.Maybe
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Reflex.Localize.Dom

import Ergvein.Crypto
import Ergvein.Either
import Ergvein.Text
import Ergvein.Types.Restore
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Sepulcas.Elements
import Sepulcas.Elements.Input
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Localization.Seed
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Resize
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper

import qualified Data.List      as L
import qualified Data.Serialize as S
import qualified Data.Text      as T
import qualified Data.Vector    as V

mnemonicPage :: MonadFrontBase t m => m ()
mnemonicPage = go Nothing
  where
    go mnemonic = wrapperSimple True $ do
      (e, mnemonicD) <- mnemonicWidget mnemonic
      void $ nextWidget $ ffor e $ \mn -> Retractable {
          retractableNext = checkPage mn
        , retractablePrev = Just $ go <$> mnemonicD
        }

checkPage :: MonadFrontBase t m => Mnemonic -> m ()
checkPage mnemonic = wrapperSimple True $ do
  mnemonicE <- mnemonicCheckWidget mnemonic
  void $ nextWidget $ ffor mnemonicE $ \mnemonic' -> Retractable {
      retractableNext = selectCurrenciesPage WalletGenerated mnemonic'
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
      elClass "span" "mnemonic-word-ix" $ text $ showt i
      text w

    smallMnemonic phrase = flip traverse_ (zip [(1 :: Int)..] . T.words $ phrase) $ uncurry (wordColumn "mnemonic-word-mb")
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

-- | Interactive check of mnemonic phrase
mnemonicCheckWidget :: MonadFrontBase t m => Mnemonic -> m (Event t Mnemonic)
mnemonicCheckWidget mnemonic = mdo
  let ws = T.words mnemonic
  langD <- getLanguage
  divClass "mnemonic-verify-title" $ h4 $ localizedText SPSVerifyTitle
  idyn <- holdDyn 0 ie
  h4 $ dynText $ do
    l <- langD
    i <- idyn
    pure $ localizedShow l $ SPSSelectWord (i+1)
  ie <- guessButtons ws idyn
  pure $ fforMaybe (updated idyn) $ \i -> if i >= length ws
    then Just mnemonic
    else Nothing

guessButtons :: forall t m . MonadFrontBase t m => [Text] -> Dynamic t Int -> m (Event t Int)
guessButtons ws idyn = do
  resD <- networkHoldDyn $ ffor idyn $ \i -> if i >= length ws
    then pure never else divClass "guess-buttons grid3" $ do
      let correctWord = ws !! i
      fakeWord1 <- randomPick [correctWord]
      fakeWord2 <- randomPick [correctWord, fakeWord1]
      wordsList <- shuffle [correctWord, fakeWord1, fakeWord2]
      fmap leftmost $ traverse (guessButton i (correctWord)) wordsList
  pure $ switch . current $ resD
  where
    fact i = product [1 .. i]
    randomPick bs = do
      i <- liftIO $ getRandomR (0, length wordListEnglish - 1)
      let word = wordListEnglish V.! i
      if word `elem` bs then randomPick bs else pure word
    shuffle is = liftIO $ do
      i <- getRandomR (0, fact (length is) - 1)
      pure $ permutations is !! i
    guessButton :: Int -> Text -> Text -> m (Event t Int)
    guessButton i correctWord buttonWord = mdo
      classeD <- holdDyn "button button-outline guess-button" $ ffor btnE $ const $
        "button guess-button " <> if buttonWord == correctWord then "guess-true" else "guess-false"
      btnE <- buttonClass classeD $ buttonWord
      delay 1 $ fforMaybe btnE $ const $ if buttonWord == correctWord then Just (i + 1) else Nothing

pasteBtn :: MonadFrontBase t m => m (Event t ())
pasteBtn = outlineTextIconButtonTypeButton CSPaste "fas fa-clipboard fa-lg"

scanQRBtn :: MonadFrontBase t m => m (Event t ())
scanQRBtn = outlineTextIconButtonTypeButton CSScanQR "fas fa-qrcode fa-lg"

askSeedPasswordPage :: MonadFrontBase t m => EncryptedByteString -> m ()
askSeedPasswordPage encryptedMnemonic = do
  passE <- askTextPasswordPage PPSMnemonicUnlock ("" :: Text)
  let mnemonicBSE = (decryptBSWithAEAD encryptedMnemonic) <$> passE
  verifiedMnemonicE <- handleDangerMsg mnemonicBSE
  void $ nextWidget $ ffor (decodeUtf8With lenientDecode <$> verifiedMnemonicE) $ \mnem -> Retractable {
      retractableNext = selectCurrenciesPage WalletRestored mnem
    , retractablePrev = Just $ pure $ askSeedPasswordPage encryptedMnemonic
    }

-- | Word by word seed restore.
-- This is not used now. Keep just in case we decide to enable it again
seedRestoreWidget :: forall t m . MonadFrontBase t m => m (Event t Mnemonic)
seedRestoreWidget = mdo
  langD <- getLanguage
  ixD <- foldDyn (\_ i -> i + 1) 1 wordE
  h4 $ dynText $
    localizedShow <$> langD <*> (SPSEnterWord <$> ixD)
  suggestionsD <- holdDyn Nothing $ ffor (updated inputD) $ \t -> if t == ""
    then Nothing else Just $ take 6 $ getWordsWithPrefix $ T.toLower t
  btnE <- fmap switchDyn $ networkHoldDyn $ ffor suggestionsD $ \case
    Nothing -> waiting
    Just ws -> divClass "restore-seed-buttons-wrapper" $ fmap leftmost $ flip traverse ws $ \w -> do
      btnClickE <- buttonClass (pure "button button-outline") w
      pure $ w <$ btnClickE
  let enterPressedE = keypress Enter txtInput
      inputD = _inputElement_value txtInput
      enterE = flip push enterPressedE $ const $ do
        sugs <- sampleDyn suggestionsD
        pure $ case sugs of
          Just (w:[]) -> Just w
          _ -> Nothing
      wordE = leftmost [btnE, enterE]
  txtInput <- textInput $ def & inputElementConfig_setValue .~ fmap (const "") wordE
  mnemD <- foldDyn (\w m -> let p = if m == "" then "" else " " in m <> p <> (T.toLower w)) "" wordE
  goE <- delay 0.1 (updated ixD)
  pure $ attachWithMaybe (\mnem i -> if i == 25 then Just mnem else Nothing) (current mnemD) goE
  where
    waiting :: m (Event t Text)
    waiting = (h4 $ localizedText SPSWaiting) >> pure never

seedRestorePage :: MonadFrontBase t m => m ()
seedRestorePage = wrapperSimple True $ do
  h2 $ localizedText SPSTypeTitle
  let cls = "ml-a mr-a mb-2 w-80" <> if isAndroid then " fit-content" else " navbar-2-cols"
      btnCls = "button button-outline" <>
        if isAndroid then " disp-block w-100" else ""
  goE <- divClass cls $ do
    e1 <- fmap (True <$)  $ buttonClass btnCls SPSPlain
    e2 <- fmap (False <$) $ buttonClass btnCls SPSBase58
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
    fmap leftmost $ traverse mkBtn [24,21,18,15,12]
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
  _ -> if length ws == l
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

  let parsedD = parseMnem mnemLength <$> _inputElement_value ti
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
  ti <- textInput $ def & inputElementConfig_setValue .~ setValE
  let setValE = leftmost [pasteE, fillE, resetE]
  let tiEl = _element_raw $ _inputElement_element ti
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
      w:[] -> divClass "restore-seed-buttons-wrapper" $ do
        let res = recombine ts w
        clickE <- buttonClass (pure "button button-outline") w
        pure $ res <$ leftmost [clickE, enterKeyE]
      _ -> do
        let ws' = take 6 ws
        wE <- divClass "restore-seed-buttons-wrapper" $ fmap leftmost $ flip traverse ws' $ \w -> do
          btnClickE <- buttonClass (pure "button button-outline") w
          pure $ w <$ btnClickE
        pure $ recombine ts <$> wE
  pasteE <- pasteBtnsWidget
  void $ networkHold (pure ()) $ ffor (updated stateD ) $ \case
    PSDone mnem -> do
      submitE <- outlineButton CSForward
      void $ nextWidget $ ffor submitE $ const $ Retractable {
          retractableNext = selectCurrenciesPage WalletRestored mnem
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
      void $ divClass "mb-1" $ flip traverse errs $
        divClass "" . localizedText . SPSMisspelledWord
      pure never

base58RestorePage :: MonadFrontBase t m => m ()
base58RestorePage = wrapperSimple True $ mdo
  h4 $ localizedText $ SPSBase58Title
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
