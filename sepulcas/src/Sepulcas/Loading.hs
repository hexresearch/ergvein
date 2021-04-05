{-# LANGUAGE OverloadedLists #-}
module Sepulcas.Loading(
    loadingWidget
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor
import Data.Time
import Reflex.Dom
import Reflex.Network
import Sepulcas.Monad
import Sepulcas.Text

percentDyn :: (PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadHold t m, MonadFix m, MonadIO (Performable m)) => NominalDiffTime -> m (Dynamic t Int)
percentDyn dt = do
  buildE <- getPostBuild
  (eE, fire) <- newTriggerEvent
  tickE <- delay dt $ leftmost [buildE, eE]
  performEvent_ $ (liftIO $ fire ()) <$ tickE
  foldDyn (\_ v -> if v == 100 then 0 else v + 10) 0 tickE

loadingWidget :: forall t m . Sepulcable t m => m ()
loadingWidget = do
  errE <- newAlertEvent
  triggerE <- fmap fst getLoadingWidgetTF
  backE <- fmap fst getBackEventFire
  let toggleE = leftmost [(False, "") <$ backE, (False, "") <$ errE, triggerE]
  tglD <- holdUniqDyn =<< holdDyn (False, "") toggleE
  void $ networkHold (pure ()) $ ffor (updated tglD) $ \(b, t) -> if not b
    then pure ()
    else divClass "loading-page" $ divClass "loading-box" $ do
      el "h5" $ text t
      divClass "loading__bar" $ do
        percD <- percentDyn 0.15
        let attrD' = ffor percD $ \v -> [
                ("class", "loading__status")
              , ("style", "width: " <> showt v <> "%;")]
        elDynAttr "div" attrD' $ pure ()
