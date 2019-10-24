-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicPage
  , mnemonicWidget
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
      divClass "mnemonic-title" $ h4 $ text "Theese words are your seed phrase"
      divClass "mnemonic-colony" $ colonize 4 (prepareMnemonic 4 phrase) $ \(i,w) ->
        divClass "column mnemonic-word" $ do
          elClass "span" "mnemonic-word-ix" $ text $ showt i
          text w
      divClass "mnemonic-warn" $ h4 $ text "It is the ONLY way to restore access to your wallet. Write it down or you will lost your money forever."
      btnE <- outlineButton $ pure "I wrote them"
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
