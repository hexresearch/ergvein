module Ergvein.Wallet.Localize.TestnetDisclaimer
  (
    TestnetDisclaimerString(..)
  ) where

import Ergvein.Wallet.Language

data TestnetDisclaimerString = TestnetDisclaimerLabel | TestnetDisclaimerText | TestnetDisclaimerClose

instance LocalizedPrint TestnetDisclaimerString where
  localizedShow l v = case l of
    English -> case v of
      TestnetDisclaimerLabel -> "Attention"
      TestnetDisclaimerText -> "The wallet operates on test network. Test coins are completely separated from main network and don't contain any value."
      TestnetDisclaimerClose -> "OK"
    Russian -> case v of
      TestnetDisclaimerLabel -> "Внимание"
      TestnetDisclaimerText -> "Кошелёк работает в тестовой сети. Тестовые монеты никак не связаны с основной сетью и не обладают ценностью."
      TestnetDisclaimerClose -> "ОК"
