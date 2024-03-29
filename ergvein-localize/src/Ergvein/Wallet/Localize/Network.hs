module Ergvein.Wallet.Localize.Network(
    NetworkPageStrings(..)
  ) where

import Ergvein.Index.Protocol.Types (ProtocolVersion, showProtocolVersion)
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language

import Data.Maybe (fromMaybe)
import Data.Time

data NetworkPageStrings
  = NPSTitle
  | NPSServer
  | NPSServerVal Int
  | NPSServerDescr
  | NPSSyncStatus
  | NPSNoServerAvail
  | NPSSyncInfo (Maybe BlockHeight, BlockHeight)
  | NPSDesync
  | NPSSyncDescr
  | NPSRefresh
  | NPSServerListView
  | NPSServerListInfo Currency
  | NPSServerEdit
  | NPSParseError
  | NPSDuplicateURL
  | NPSLatency NominalDiffTime
  | NPSAvgLat NominalDiffTime
  | NPSOffline
  | NPSNoIndex Currency
  | NPSHeightInfo1 (Maybe BlockHeight)
  | NPSHeightInfo2 BlockHeight
  | NPSNodes
  | NPSNodesNum Int
  | NPSActiveNum Int
  | NPSNoActiveNodes
  | NPSNoCurrencies
  | NPSTitleCur Currency
  | NPSIndexerVersion (Maybe ProtocolVersion)

instance LocalizedPrint NetworkPageStrings where
  localizedShow l v = case l of
    English -> case v of
      NPSTitle              -> "Network"
      NPSServer             -> "Servers: "
      NPSServerVal n        -> showt n <> if n == 1 then " active server" else " active servers"
      NPSServerDescr        -> "Number of servers in the list"
      NPSSyncStatus         -> "Height status: "
      NPSNoServerAvail      -> "No indexer available"
      NPSSyncInfo (ch,ah)   -> "Indexers are in sync: " <> (maybe "loading..." showt ch) <> "/" <> showt ah
      NPSDesync             -> "Indexers are not in sync"
      NPSSyncDescr          -> "Indexed height / actual blockchain height"
      NPSRefresh            -> "Refresh"
      NPSServerListView     -> "View"
      NPSServerListInfo c   -> showt c <> " indexers"
      NPSServerEdit         -> "Edit"
      NPSParseError         -> "Failed to parse URL"
      NPSDuplicateURL       -> "Duplicate URL"
      NPSLatency lat        -> "Latency: " <> showt lat
      NPSAvgLat lat         -> "Average latency: " <> showt lat
      NPSOffline            -> "Offline"
      NPSNoIndex c          -> "Has no index for " <> showt c
      NPSHeightInfo1 ch     ->
        case ch of
          Just h  -> "Index height: " <> showt h
          Nothing -> "Not indexed"
      NPSHeightInfo2 ah     -> "Blockchain height: " <> showt ah
      NPSNodes              -> "Blockchain nodes: "
      NPSNodesNum n         -> "Total nodes: " <> showt n
      NPSActiveNum n        -> showt n <> " connected"
      NPSNoActiveNodes      -> "No active nodes"
      NPSNoCurrencies       -> "No active currencies"
      NPSTitleCur c         -> "Network " <> showt c
      NPSIndexerVersion ver -> "Indexer version: " <> (showProtocolVersion $ fromMaybe (0,0,0) ver)
    Russian -> case v of
      NPSTitle              -> "Сеть"
      NPSServer             -> "Сервера: "
      NPSServerVal n        -> (<>) (showt n <> " активных ") $ case n `div` 10 of
                                0 -> "серверов"
                                1 -> "сервер"
                                2 -> "сервера"
                                3 -> "сервера"
                                4 -> "сервера"
                                _ -> "серверов"
      NPSServerDescr        -> "Количество серверов в списке"
      NPSSyncStatus         -> "Высота: "
      NPSNoServerAvail      -> "Нет доступных индексеров"
      NPSSyncInfo (ch,ah)   -> "Индексеры синхронизованы: " <> showt ch <> "/" <> showt ah
      NPSDesync             -> "Индексеры не синхронизованы"
      NPSSyncDescr          -> "Индексированная высота / действительная высота блокчейна"
      NPSRefresh            -> "Обновить"
      NPSServerListView     -> "Обзор"
      NPSServerListInfo c   -> showt c <> " индексеры"
      NPSServerEdit         -> "Редактировать"
      NPSParseError         -> "Некорректный URL"
      NPSDuplicateURL       -> "Дублирование URL"
      NPSLatency lat        -> "Задержка: " <> showt lat
      NPSAvgLat lat         -> "Средняя задержка : " <> showt lat
      NPSOffline            -> "Оффлайн"
      NPSNoIndex c          -> "Нет индекса для " <> showt c
      NPSHeightInfo1 ch     ->
        case ch of
          Just h  -> "Высота индекса: " <> showt h
          Nothing -> "Не индексировалась"
      NPSHeightInfo2 ah     -> "Высота блокчейна: " <> showt ah
      NPSNodes              -> "Блокчейн-узлы: "
      NPSNodesNum n         -> "Всего узлов: " <> showt n
      NPSActiveNum n        -> showt n <> " активных соединений"
      NPSNoActiveNodes      -> "Нет активных узлов"
      NPSNoCurrencies       -> "Нет активных валют"
      NPSTitleCur c         -> "Сеть " <> showt c
      NPSIndexerVersion ver -> "Версия индексера: " <> (showProtocolVersion $ fromMaybe (0,0,0) ver)
