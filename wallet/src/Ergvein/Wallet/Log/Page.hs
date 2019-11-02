{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Log.Page(
    logsPage
  ) where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (pack)
import Data.Time
import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Log.Reader
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Util
import Reflex.Dom


import qualified Data.Text as T

copyButton = undefined

-- | Widget with continuation that allows to select contact and go to next step
logsPage :: forall t m . MonadFrontBase t m => m ()
logsPage = divClass "logs-page" $ mdo
  _ <- copyButton entriesTxtD
  entriesD <- logsWidget
  let entriesTxtD = T.unlines . fmap showt <$> entriesD
  pure ()
  -- pure RetractableWidget {
  --     retractableNextRoute = never
  --   , retractableResult = pure LogsPage
  --   , retractableReturn = pure logsPage
  --   , retractableBackE = never
  --   , retractableClearE = never
  --   }

-- | Display logs
logsWidget :: forall t m . MonadFrontBase t m => m (Dynamic t [LogEntry])
logsWidget = elClass "table" "log-content" $ do
  entriesD <- logReader
  void $ widgetHoldDyn $ ffor entriesD $ traverse_ (uncurry renderItem) . zip [1 ..] . take 100 -- TODO: make dynamic loading on scrolling
  pure entriesD
  where
    renderItem i LogEntry{..} = elClass "tr" "log-item-line" $ do
      elAttr "td" [("valign", "top")] $ spanClass "log-item-num" $ text . showt $ i
      el "td" $ do
        spanClass "log-item-time" $ text . showEntryTime $ logTime
        unless (null logNameSpace) $ spanClass "log-item-namespace" $ text . T.intercalate "." $ logNameSpace
        spanClass (severityToClass logSeverity) $ text logMessage

severityToClass :: LogSeverity -> Text
severityToClass v = case v of
  LogDebug -> "log-item-debug"
  LogInfo -> "log-item-info"
  LogWarning -> "log-item-warning"
  LogError -> "log-item-error"

showEntryTime :: UTCTime -> Text
showEntryTime = pack . formatTime defaultTimeLocale "%X"
