-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicPage
  , mnemonicWidget
  , seedRestorePage
  , seedRestorePageText
  ) where

import Control.Monad.Random.Strict
import Data.Bifunctor
import Data.Either (either)
import Data.List (permutations)
import Ergvein.Crypto.Keys
import Ergvein.Crypto.Util
import Ergvein.Crypto.WordLists
import Ergvein.Text
import Ergvein.Types.Restore
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.Seed
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Resize
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper
import Reflex.Localize

import qualified Data.List   as L
import qualified Data.Text   as T
import qualified Data.Vector as V

mnemonicPage :: MonadFrontBase t m => m ()
mnemonicPage = go Nothing
  where
    go mnemonic = wrapperSimple True $ do
      (e, md) <- mnemonicWidget mnemonic
      nextWidget $ ffor e $ \mn -> Retractable {
          retractableNext = checkPage mn
        , retractablePrev = Just $ go <$> md
        }
      pure ()

checkPage :: MonadFrontBase t m => Mnemonic -> m ()
checkPage mn = wrapperSimple True $ do
  e <- mnemonicCheckWidget mn
  nextWidget $ ffor e $ \m -> Retractable {
      retractableNext = selectCurrenciesPage WalletGenerated m
    , retractablePrev = Just $ pure $ checkPage m
    }
  pure ()

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
      divClass "mnemonic-colony" $ adaptive3 (smallMnemonic phrase) (mediumMnemonic phrase) (desktopMnemonic phrase)
      divClass "mnemonic-warn" $ h4 $ localizedText SPSWarn
      btnE <- outlineButton SPSWrote
      pure (phrase <$ btnE, pure $ Just phrase)
  where
    prepareMnemonic :: Int -> Mnemonic -> [(Int, Text)]
    prepareMnemonic cols = L.concat . L.transpose . mkCols cols . zip [1..] . T.words

    wordColumn cs i w = divClass ("column " <> cs) $ do
      elClass "span" "mnemonic-word-ix" $ text $ showt i
      text w

    smallMnemonic phrase = flip traverse_ (zip [1..] . T.words $ phrase) $ uncurry (wordColumn "mnemonic-word-mb")
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
  resD <- widgetHoldDyn $ ffor idyn $ \i -> if i >= length ws
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

seedRestorePage :: forall t m . MonadFrontBase t m => m ()
seedRestorePage = wrapperSimple True $ do
  h4 $ localizedText SPSRestoreTitle
  resetE <- buttonClass (pure "button button-outline") SPSReset
  mnemE <- fmap (switch . current) $ widgetHold seedRestoreWidget $ seedRestoreWidget <$ resetE
  void $ nextWidget $ ffor mnemE $ \m -> Retractable {
      retractableNext = selectCurrenciesPage WalletRestored m
    , retractablePrev = Just $ pure seedRestorePage
    }

-- | For debug purposes. Instead of writing the phrase one word at a time, paste the whole phrase instantly
-- Switch seedRestorePage to seedRestorePageText at Ergvein.Wallet.Page.Initial:48 to enable.
seedRestorePageText :: MonadFrontBase t m => m ()
seedRestorePageText = wrapperSimple True $ do
  h4 $ localizedText SPSRestoreTitle
  textD <- textFieldNoLabel ""
  mnemE <- fmap (switch . current) $ widgetHoldDyn $ ffor textD $ \t -> if length (T.words t) == 24
    then do
      goE <- buttonClass (pure "button button-outline") ("Restore" :: Text)
      pure $ t <$ goE
    else pure never
  void $ nextWidget $ ffor mnemE $ \m -> Retractable {
      retractableNext = selectCurrenciesPage WalletRestored m
    , retractablePrev = Just $ pure seedRestorePageText
    }

seedRestoreWidget :: forall t m . MonadFrontBase t m => m (Event t Mnemonic)
seedRestoreWidget = mdo
  langD <- getLanguage
  ixD <- foldDyn (\_ i -> i + 1) 1 wordE
  h4 $ dynText $
    localizedShow <$> langD <*> (SPSEnterWord <$> ixD)
  suggestionsD <- holdDyn Nothing $ ffor (updated inputD) $ \t -> if t == ""
    then Nothing else Just $ take 6 $ getWordsWithPrefix $ T.toLower t
  btnE <- fmap switchDyn $ widgetHoldDyn $ ffor suggestionsD $ \case
    Nothing -> waiting
    Just ws -> divClass "restore-seed-buttons-wrapper" $ fmap leftmost $ flip traverse ws $ \w -> do
      btnClickE <- buttonClass (pure "button button-outline") w
      pure $ w <$ btnClickE
  let emptyStr :: Text = ""
      enterPressedE = keypress Enter txtInput
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

validateSeedWord :: Text -> Either [SeedPageStrings] Text
validateSeedWord word = if wordTrieElem $ T.toLower word
  then Right word
  else Left [SPSInvalidWord]
