-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicPage
  , mnemonicWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad

import qualified Data.Text as T

mnemonicPage :: MonadFront t m => m ()
mnemonicPage = container $ do
  _ <- mnemonicWidget
  pure ()

mnemonicWidget :: MonadFront t m => m (Event t Text)
mnemonicWidget = do
  phrase <- pure $ T.unwords mockSeed
  divClass "mnemonic-title" $ h4 $ text "Theese words are your seed phrase"
  colonize 4 (T.words phrase) $ divClass "column mnemonic-word" . text
  divClass "mnemonic-warn" $ h4 $ text "It is the ONLY way to restore access to your wallet. Write it down or you will lost your money forever."
  btnE <- buttonClass "button button-outline" $ pure "I wrote them"
  pure $ phrase <$ btnE

mockSeed :: [Text]
mockSeed = [ "inflict", "rose", "twelve", "coach", "elder", "live", "demand"
  , "nurse", "clump", "claim", "pave", "detect", "guard", "rescue", "quantum"
  , "devote", "quote", "reflect", "found", "turtle", "portion", "option"
  , "resemble", "maple"]
