module Ergvein.Wallet.Localization.Network(
    NetworkPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data NetworkPageStrings =
    NPSTitle
  | NPSStatus
  | NPSStatusVal Int
  | NPSStatusDescr

instance LocalizedPrint NetworkPageStrings where
  localizedShow l v = case l of
    English -> case v of
      NPSTitle        -> "Network"
      NPSStatus       -> "Status: "
      NPSStatusVal n  -> showt n <> " connections."
      NPSStatusDescr  -> "Amount of indexers connected"
    Russian -> case v of
      NPSTitle        -> "Сеть"
      NPSStatus       -> "Статус: "
      NPSStatusVal n  -> (<>) (showt n) $ case n of
                            0 -> " соединений."
                            1 -> " соединение."
                            2 -> " соединения."
                            3 -> " соединения."
                            4 -> " соединения."
                            _ -> " соединений."
      NPSStatusDescr  -> "Количество подключенных индексаторов"
