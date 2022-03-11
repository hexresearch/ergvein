{-# OPTIONS_GHC -Wno-orphans #-}
module Sepulcas.Monad.Reflex(
    module Sepulcas.Monad.Reflex
  , module Reflex
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Ref
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Host.Class
import Reflex.Spider.Internal (SpiderHostFrame, Global)
import Sepulcas.Native

import qualified Control.Monad.Fail as F
import qualified Reflex.Profiled as RP

-- | Type classes that we need from reflex-dom itself.
type MonadReflex t m =
  ( Reflex t
  , MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadUnliftIO (Performable m)
  , MonadSample t (Performable m)
  , MonadJSM (Performable m)
  , F.MonadFail (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , PlatformNatives
  , MonadReflexCreateTrigger t m
  )

instance F.MonadFail (WithJSContextSingleton x (SpiderHostFrame Global)) where
  fail = liftIO . F.fail

instance F.MonadFail (WithJSContextSingleton x (RP.ProfiledM (SpiderHostFrame Global))) where
  fail = liftIO . F.fail
