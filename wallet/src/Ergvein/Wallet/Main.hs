module Ergvein.Wallet.Main(
    frontend
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Seed

import Ergvein.Wallet.Embed
import Ergvein.Wallet.Style.TH

frontend :: MonadFront t m => m ()
frontend = do
  url <- createObjectURL testImg
  elAttr "img" ("src" =: url) blank
  void mnemonicPage
