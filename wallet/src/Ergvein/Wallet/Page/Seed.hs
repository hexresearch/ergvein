-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicPage
  , mnemonicWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad

mnemonicPage :: MonadFront t m => m ()
mnemonicPage = container $ do
  _ <- mnemonicWidget
  pure ()

mnemonicWidget :: MonadFront t m => m (Event t Text)
mnemonicWidget = divClass "mnemonic" $ do
  traverse_ (spanClass "mnemonic-word" . text) mockSeed
  pure never

mockSeed :: [Text]
mockSeed = [ "inflict", "rose", "twelve", "coach", "elder", "live", "demand"
  , "nurse", "clump", "claim", "pave", "detect", "guard", "rescue", "quantum"
  , "devote", "quote", "reflect", "found", "turtle", "portion", "option"
  , "resemble", "maple"]
