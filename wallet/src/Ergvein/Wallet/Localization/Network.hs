module Ergvein.Wallet.Localization.Network(
    NetworkPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language

import Data.Text
import Servant.Client (BaseUrl, showBaseUrl)

data NetworkPageStrings =
    NPSTitle
  | NPSStatus
  | NPSStatusVal Int
  | NPSStatusDescr
  | NPSServer
  | NPSServerVal BaseUrl
  | NPSServerValEdit
  | NPSServerDescr
  | NPSHeight
  | NPSHeightVal BlockHeight
  | NPSHeightDescr
  | NPSWait
  | NPSNoValue
  | forall e . LocalizedPrint e => NPSError e
  | NPSSelectServer Currency

instance LocalizedPrint NetworkPageStrings where
  localizedShow l v = case l of
    English -> case v of
      NPSTitle          -> "Network"
      NPSStatus         -> "Status: "
      NPSStatusVal n    -> showt n <> " connections."
      NPSStatusDescr    -> "Amount of indexers connected"
      NPSServer         -> "Server: "
      NPSServerVal s    -> pack $ showBaseUrl s
      NPSServerValEdit  -> "Edit"
      NPSServerDescr    -> "Server indexer for tx history"
      NPSHeight         -> "Height: "
      NPSHeightVal n    -> showt n <> " blocks."
      NPSHeightDescr    -> "Current height (and if there any forks detected)"
      NPSWait           -> "Wait ..."
      NPSNoValue        -> "No value"
      NPSError e        -> localizedShow l e
      NPSSelectServer c -> "Select server for " <> showt c
    Russian -> case v of
      NPSTitle          -> "Сеть"
      NPSStatus         -> "Статус: "
      NPSStatusVal n    -> (<>) (showt n) $ case n of
                              0 -> " соединений."
                              1 -> " соединение."
                              2 -> " соединения."
                              3 -> " соединения."
                              4 -> " соединения."
                              _ -> " соединений."
      NPSStatusDescr    -> "Количество подключенных индексаторов"
      NPSServer         -> "Сервер: "
      NPSServerVal s    -> pack $ showBaseUrl s
      NPSServerValEdit  -> "Редактировать"
      NPSServerDescr    -> "Сервер индесатор для истории транзаций"
      NPSHeight         -> "Высота: "
      NPSHeightVal n    -> (<>) (showt n) $ case n of
                              0 -> " блоков."
                              1 -> " блок."
                              2 -> " блока."
                              3 -> " блока."
                              4 -> " блока."
                              _ -> " блоков."
      NPSHeightDescr    -> "Текущая высота (и форки, если обнаружены)"
      NPSWait           -> "Ждите ..."
      NPSNoValue        -> "Нет значения"
      NPSError e        -> localizedShow l e
      NPSSelectServer c -> "Выберите сервер для " <> showt c
