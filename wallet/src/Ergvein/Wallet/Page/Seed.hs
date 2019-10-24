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
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Input

import qualified Data.Text as T

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
      divClass "mnemonic-title" $ h4 $ text "Theese words are your seed phrase"
      colonize 4 (T.words phrase) $ divClass "column mnemonic-word" . text
      divClass "mnemonic-warn" $ h4 $ text "It is the ONLY way to restore access to your wallet. Write it down or you will lost your money forever."
      btnE <- outlineButton $ pure "I wrote them"
      pure (phrase <$ btnE, pure $ Just phrase)

-- | Interactive check of mnemonic phrase
mnemonicCheckWidget :: MonadFront t m => Mnemonic -> m (Event t Mnemonic)
mnemonicCheckWidget mnemonic = mdo
  let ws = T.words mnemonic
  divClass "mnemonic-verify-title" $ h4 $ text "Double check the seed phrase"
  idyn <- holdDyn 0 ie
  divClass "mnemonic-verify-n" $ h4 $ dynText $ ffor idyn $
    \i -> "Select the " <> showt (i+1) <> "th word"
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
      btnE <- buttonClass classeD $ pure $ ws !! i
      delay 1 $ fforMaybe btnE $ const $ if reali == i then Just (i+1) else Nothing

seedRestorePage :: forall t m . MonadFront t m => m ()
seedRestorePage = do
  h4 $ text "Seed restore"
  resetE <- buttonClass (pure "button button-outline") (pure "Reset and start again")
  mnemE <- fmap (switch . current) $ widgetHold seedRestoreWidget $ seedRestoreWidget <$ resetE
  widgetHold (pure ()) $ ffor mnemE $ h4 . text
  pure ()

seedRestoreWidget :: forall t m . MonadFront t m => m (Event t Mnemonic)
seedRestoreWidget = mdo
  ixD <- foldDyn (\_ i -> i + 1) 1 wordE
  divClass "mnemonic-verify-n" $ h4 $ dynText $ ffor ixD $
    \i -> "Enter the " <> showt i <> "th word"
  wordE <- fmap (switch . current) $ widgetHold waiting $ ffor (updated inpD) $ \t -> if t == ""
    then waiting
    else fmap leftmost $ colonize 3 (take 9 $ getWordsWithPrefix t) $ \w -> do
       btnE <- buttonClass (pure "button button-outline guess-button restore-word") (pure w)
       pure $ w <$ btnE
  inpD <- fmap join $ widgetHold (textField "" "") $ ffor wordE $ const $ textField "" ""
  mnemD <- foldDyn (\w m -> let p = if m == "" then "" else " " in m <> p <> w) "" wordE
  goE <- delay 0.1 (updated ixD)
  pure $ attachWithMaybe (\mnem i -> if i == 5 then Just mnem else Nothing) (current mnemD) goE
  where
    waiting :: m (Event t Text)
    waiting = do
      h4 $ text "Waiting for input..."
      pure never
