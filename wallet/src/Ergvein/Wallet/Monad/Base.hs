module Ergvein.Wallet.Monad.Base
  (
    MonadBaseConstr
  , MonadFrontBase
  , MonadBackable(..)
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable
import Reflex.Localize
import Ergvein.Wallet.Native
import Ergvein.Wallet.Native

-- | Type classes that we need from reflex-dom itself.
type MonadBaseConstr t m = (MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadSample t (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  )

-- | Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadFrontBase t m = (PlatformNatives
  , HasStoreDir m
  , MonadBaseConstr t m
  , MonadLocalized t m
  , MonadRetract t m
  , MonadBackable t m)

class MonadBaseConstr t m => MonadBackable t m | m -> t where
  -- | System back button event
  getBackEvent :: m (Event t ())
