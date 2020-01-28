{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import Data.Function.Flip (flip3)
import Reflex.Host.Class
import Reflex.Dom as RD
--import Reflex.Dom.Canvas.Context2D    as CanvasF
--import Reflex.Dom.CanvasBuilder.Types as Canvas
--import Reflex.Dom.CanvasDyn           as CDyn

import Ergvein.Text
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Widget.GraphPinCode
import Ergvein.Wallet.Wrapper

data SubPageSettings
  = GoLanguage
  | GoPinCode

settingsPage :: MonadFront t m => m ()
settingsPage = do
  let thisWidget = Just $ pure $ settingsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    divClass "initial-options grid1" $ do
      goLangE <- fmap (GoLanguage <$) $ outlineButton STPSButLanguage
      goPinE  <- fmap (GoPinCode  <$) $ outlineButton STPSButPinCode
      let goE = leftmost [goLangE, goPinE]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage  -> languagePage
            GoPinCode   -> pinCodePage
        , retractablePrev = thisWidget
        }

languagePage :: MonadFront t m => m ()
languagePage = do
  let thisWidget = Just $ pure $ languagePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSelectLanguage
    divClass "initial-options grid1" $ do
      langD <- getLanguage
      initKey <- sample . current $ langD
      let listLangsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allLanguages
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated langD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listLangsD ddnCfg
      let selD = _dropdown_value dp
      selE <- fmap updated $ holdUniqDyn selD
      widgetHold (pure ()) $ setLanguage <$> selE
      settings <- getSettings
      updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
      pure ()
    pure ()

--data PinOnOff = PinOn | PinOff
data GoPinSets
  = GoPinBase
  | GoPinInputCode

pinCodePage :: MonadFront t m => m ()
pinCodePage = do
  let thisWidget = Just $ pure $ pinCodePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    goPinSetsE <- fmap (GoPinBase <$) getPostBuild
    rec goPinE <- fmap switchDyn $ widgetHold (pure never) $
          ffor (leftmost [goPinE, goPinSetsE]) $ \case
            GoPinBase       -> pagePinBase
            GoPinInputCode  -> pagePinInput
    --let switchE = ffor switchE'
    --pinCodeE <- graphPinCode never
    --pinCodeD <- holdDyn (PinCode []) pinCodeE
    --dynText $ fmap showt pinCodeD
    pure ()
    where
      pagePinBase :: MonadFront t m => m (Event t GoPinSets)
      pagePinBase = do
        h3 $ localizedText $ STPSSetsPinCode
        setsPinCodeMb <- fmap settingsPinCode getSettings
        switchE' <- outlineButton $ case setsPinCodeMb of
                      Nothing -> STPSSetsPinOn
                      Just _  -> STPSSetsPinOff
        let switchE = ffor switchE' $ \_ -> case setsPinCodeMb of
                        Nothing -> GoPinInputCode
                        Just _  -> GoPinBase
        let cleanE = fforMaybe switchE $ \case
                    GoPinBase -> Just ()
                    _         -> Nothing
        settings <- getSettings
        updateSettings $ ffor cleanE (\_ -> settings {settingsPinCode = Nothing})
        delay 0.1 switchE

      pagePinInput :: MonadFront t m => m (Event t GoPinSets)
      pagePinInput = do
        h3 $ localizedText $ STPSSetsPinInput
        pinCodeE <- graphPinCode never
        setPinE <- fmap switchDyn $ widgetHold (pure never) $
          ffor pinCodeE $ \PinCode{..} ->
            fmap (unPinCode <$) $ outlineButton STPSSetsPinDoSet
        settings <- getSettings
        updateSettings $ ffor setPinE (\pcv -> settings {settingsPinCode = Just $ showt pcv})
        delay 0.1 $ GoPinBase <$ setPinE

{-
graphPinCode :: forall t m . (MonadFrontBase t m) => m ()
graphPinCode = do
  elAttr "svg" [ ("style","display: block; width: 400px; height: 400px; margin-left: auto; margin-right: auto;")
               , ("width", "400"), ("height", "400"), ("viewBox","0 0 400 400")
               ] $ do
    elAttr "circle" [ ("cx","100")
                    , ("cy","100")
                    , ("r","25")
                    --, ("style","fill: #ff0000; stroke: #ff0000; stroke-width: 1;")
                    , ("stroke","green")
                    , ("fill","white")
                    , ("stroke-width","1")
                    ] blank
    elAttr "rect" [("x","50"), ("y","100"), ("width","200"), ("height","100"), ("style","fill: #ffc107; stroke: #e65100; stroke-width: 2;")] blank
    pure ()

graphPinCodeCs :: MonadFront t m => m ()
graphPinCodeCs = do
  now <- liftIO getCurrentTime
  canvasEl <- fst <$> elAttr' "canvas"
                        [ ("style","display: block; width: 400px; height: 400px; margin-left: auto; margin-right: auto;")
                        ] blank
  d2D <- fmap Canvas._canvasInfo_context <$> CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )
  eTick <- fmap void $ RD.tickLossy 0.1 now
  _ <- CDyn.nextFrameWithCxFree (constDyn toDraw) d2D eTick
  pure ()
  where
    toDraw = do
      CanvasF.strokeStyleF "#FF0000"
      CanvasF.rectF 10 10 100 100
-}
