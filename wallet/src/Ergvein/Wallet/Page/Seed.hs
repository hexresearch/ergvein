-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicPage
  , mnemonicWidget
  , seedRestorePage
  ) where

import Control.Monad.Random.Strict
import Data.Bifunctor
import Data.List (permutations)
import Ergvein.Crypto.Keys
import Ergvein.Crypto.WordLists
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.Seed
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper
import Reflex.Localize

import qualified Data.Text as T
import qualified Data.List as L

mnemonicPage :: MonadFront t m => m ()
mnemonicPage = go Nothing
  where
    go mnemonic = wrapper True $ do
      (e, md) <- mnemonicWidget mnemonic
      nextWidget $ ffor e $ \mn -> Retractable {
          retractableNext = checkPage mn
        , retractablePrev = Just $ go <$> md
        }
      pure ()

checkPage :: MonadFront t m => Mnemonic -> m ()
checkPage mn = wrapper True $ do
  e <- mnemonicCheckWidget mn
  nextWidget $ ffor e $ \m -> Retractable {
      retractableNext = passwordPage m
    , retractablePrev = Just $ pure $ checkPage m
    }
  pure ()

generateMnemonic :: MonadFront t m => m (Maybe Mnemonic)
generateMnemonic = do
  e <- liftIO getEntropy
  validateNow $ first T.pack $ toMnemonic e

-- | Generate and show mnemonic phrase to user. Returned dynamic is state of widget.
mnemonicWidget :: MonadFront t m => Maybe Mnemonic -> m (Event t Mnemonic, Dynamic t (Maybe Mnemonic))
mnemonicWidget mnemonic = do
  mphrase <- maybe generateMnemonic (pure . Just) mnemonic
  case mphrase of
    Nothing -> pure (never, pure Nothing)
    Just phrase -> do
      divClass "mnemonic-title" $ h4 $ localizedText SPSTitle
      divClass "mnemonic-colony" $ colonize 4 (prepareMnemonic 4 phrase) $ \(i,w) ->
        divClass "column mnemonic-word" $ do
          elClass "span" "mnemonic-word-ix" $ text $ showt i
          text w
      divClass "mnemonic-warn" $ h4 $ localizedText SPSWarn
      btnE <- outlineButton SPSWrote
      pure (phrase <$ btnE, pure $ Just phrase)
  where
    prepareMnemonic :: Int -> Mnemonic -> [(Int, Text)]
    prepareMnemonic cols = L.concat . L.transpose . mkCols cols . zip [1..] . T.words

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
mnemonicCheckWidget :: MonadFront t m => Mnemonic -> m (Event t Mnemonic)
mnemonicCheckWidget mnemonic = mdo
  let ws = T.words mnemonic
  langD <- getLanguage
  divClass "mnemonic-verify-title" $ h4 $ localizedText SPSVerifyTitle
  idyn <- holdDyn 0 ie
  divClass "mnemonic-verify-n" $ h4 $ dynText $ do
    l <- langD
    i <- idyn
    pure $ localizedShow l $ SPSSelectWord (i+1)
  ie <- guessButtons ws idyn
  pure $ fforMaybe (updated idyn) $ \i -> if i >= length ws
    then Just mnemonic
    else Nothing

guessButtons :: forall t m . MonadFront t m => [Text] -> Dynamic t Int -> m (Event t Int)
guessButtons ws idyn = do
  resD <- widgetHoldDyn $ ffor idyn $ \i -> if i >= length ws
    then pure never else divClass "guess-buttons" $ do
      fake1 <- randomPick [i]
      fake2 <- randomPick [i, fake1]
      is <- shuffle [i, fake1, fake2]
      fmap leftmost $ traverse (guessButton i) is
  pure $ switch . current $ resD
  where
    fact i = product [1 .. i]
    randomPick bs = do
      i <- liftIO $ getRandomR (0, length ws - 1)
      if i `elem` bs then randomPick bs else pure i
    shuffle is = liftIO $ do
      i <- getRandomR (0, fact (length is) - 1)
      pure $ permutations is !! i
    guessButton :: Int -> Int -> m (Event t Int)
    guessButton reali i = mdo
      classeD <- holdDyn "button button-outline guess-button" $ ffor btnE $ const $
        "button guess-button " <> if reali == i then "guess-true" else "guess-false"
      btnE <- buttonClass classeD $ ws !! i
      delay 1 $ fforMaybe btnE $ const $ if reali == i then Just (i+1) else Nothing

seedRestorePage :: forall t m . MonadFront t m => m ()
seedRestorePage = do
  h4 $ localizedText SPSRestoreTitle
  resetE <- buttonClass (pure "button button-outline") SPSReset
  mnemE <- fmap (switch . current) $ widgetHold seedRestoreWidget $ seedRestoreWidget <$ resetE
  widgetHold (pure ()) $ ffor mnemE $ h4 . text
  pure ()

seedRestoreWidget :: forall t m . MonadFront t m => m (Event t Mnemonic)
seedRestoreWidget = mdo
  langD <- getLanguage
  ixD <- foldDyn (\_ i -> i + 1) 1 wordE
  divClass "mnemonic-verify-n" $ h4 $ dynText $
    localizedShow <$> langD <*> (SPSEnterWord <$> ixD)
  wordE <- fmap (switch . current) $ widgetHold waiting $ ffor (updated inpD) $ \t -> if t == ""
    then waiting
    else fmap leftmost $ colonize 3 (take 9 $ getWordsWithPrefix t) $ \w -> do
       btnE <- buttonClass (pure "button button-outline guess-button restore-word") w
       pure $ w <$ btnE
  let emptyStr :: Text = ""
  inpD <- fmap join $ widgetHold (textField emptyStr "") $ ffor wordE $ const $ textField emptyStr ""
  mnemD <- foldDyn (\w m -> let p = if m == "" then "" else " " in m <> p <> w) "" wordE
  goE <- delay 0.1 (updated ixD)
  pure $ attachWithMaybe (\mnem i -> if i == 5 then Just mnem else Nothing) (current mnemD) goE
  where
    waiting :: m (Event t Text)
    waiting = (h4 $ localizedText SPSWaiting) >> pure never
