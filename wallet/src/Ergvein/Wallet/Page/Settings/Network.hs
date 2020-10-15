{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Settings.Network
  (
    networkSettingsPage
  , networkSettingsPageUnauth
  ) where

import Control.Lens
import Data.Functor.Misc (Const2(..))
import Data.Maybe (isJust)
import Network.Socket
import Reflex.Dom
import Reflex.ExternalRef
import Text.Read

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Indexer.Socket
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S

data NavbarItem = ActivePage | DisabledPage | ParametersPage
  deriving (Eq)

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      ActivePage      -> "Active indexers"
      DisabledPage    -> "Reserved indexers"
      ParametersPage  -> "Network parameters"
    Russian -> case v of
      ActivePage      -> "Используемые индексеры"
      DisabledPage    -> "Запасные индексеры"
      ParametersPage  -> "Сетевые параметры"

data ParametersParseErrors = PPENDT | PPEInt

instance LocalizedPrint ParametersParseErrors where
  localizedShow l v = case l of
    English -> case v of
      PPENDT -> "Failed to parse seconds"
      PPEInt -> "Failed to parse integer"
    Russian -> case v of
      PPENDT -> "Некорректное значение. Только дробные числа"
      PPEInt -> "Некорректное значение. Только целые числа"

networkSettingsPage :: MonadFront t m => m ()
networkSettingsPage = do
  title <- localized NSSTitle
  wrapper False title (Just $ pure networkSettingsPage ) $ do
    navD <- navbarWidget ActivePage
    void $ widgetHoldDyn $ ffor navD $ \case
      ActivePage      -> activePageWidget
      DisabledPage    -> inactivePageWidget
      ParametersPage  -> parametersPageWidget

networkSettingsPageUnauth :: MonadFrontBase t m => m ()
networkSettingsPageUnauth = wrapperSimple False $ do
  navD <- navbarWidget ActivePage
  void $ widgetHoldDyn $ ffor navD $ \case
    ActivePage      -> activePageWidget
    DisabledPage    -> inactivePageWidget
    ParametersPage  -> parametersPageWidget

parametersPageWidget :: MonadFrontBase t m => m ()
parametersPageWidget = mdo
  setD <- getSettingsD
  valsD <- fmap join $
    widgetHoldDyn $ ffor setD $ \Settings{..} -> do
      let dt0 :: Double = realToFrac settingsReqTimeout
      dtD <- fmap2 realToFrac $ textFieldValidated NSSReqTimeout dt0 $
        maybe (Left [PPENDT]) Right . readMaybe . T.unpack
      actNumD <- textFieldValidated NSSActUrlNum settingsActUrlNum $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      rminD <- textFieldValidated NSSReqNumMin (fst settingsReqUrlNum) $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      rmaxD <- textFieldValidated NSSReqNumMax (snd settingsReqUrlNum) $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      pure $ (,,,) <$> dtD <*> actNumD <*> rminD <*> rmaxD
  divClass "net-btns-2" $ do
    saveE <- buttonClass "button button-outline" NSSSave
    defE <- buttonClass "button button-outline" NSSRestoreDef
    updE <- updateSettings $ flip pushAlways defE $ const $ do
      stngs <- sample $ current setD
      pure $ stngs {
            settingsReqTimeout = defaultIndexerTimeout
          , settingsReqUrlNum  = defaultIndexersNum
          , settingsActUrlNum  = defaultActUrlNum
        }
    updE' <- updateSettings $ flip pushAlways saveE $ const $ do
      stngs <- sample $ current setD
      (dt, actNum, rmin, rmax) <- sample $ current valsD
      pure $ stngs {
            settingsReqTimeout = dt
          , settingsReqUrlNum  = (rmin, rmax)
          , settingsActUrlNum  = actNum
        }
    showSuccessMsg $ STPSSuccess <$ (leftmost [updE, updE'])
  pure ()
  where
    fmap2 = fmap . fmap

addUrlWidget :: forall t m . MonadFrontBase t m => Dynamic t Bool -> m (Event t NamedSockAddr)
addUrlWidget showD = fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b -> if not b then pure never else do
  murlE <- divClass "mt-3" $ do
    textD <- fmap _inputElement_value $ inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
    goE <- outlineButton NSSAddUrl
    rs <- mkResolvSeed
    performFork $ ffor goE $ const $ do
      t <- sampleDyn textD
      parseSingleSockAddr rs t
  void $ widgetHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors" $ localizedText NPSParseError
    _ -> pure ()
  pure $ fmapMaybe id murlE

activePageWidget :: forall t m . MonadFrontBase t m => m ()
activePageWidget = mdo
  connsD  <- externalRefDynamic =<< getActiveConnsRef
  addrsD  <- (fmap . fmap) S.toList $ externalRefDynamic =<< getActiveAddrsRef
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  let valsD = (,) <$> connsD <*> addrsD
  void $ widgetHoldDyn $ ffor valsD $ \(conmap, urls) ->
    flip traverse urls $ \sa -> renderActive sa refrE $ M.lookup (namedAddrSock sa) conmap
  hideE <- activateURL =<< addUrlWidget showD
  (refrE, tglE, e) <- divClass "network-wrapper mt-3" $ divClass "net-btns-3" $ do
    refrE' <- buttonClass "button button-outline m-0" NSSRefresh
    restoreE <- buttonClass "button button-outline m-0" NSSRestoreUrls
    rs <- mkResolvSeed
    void $ activateURLList =<< performFork (parseSockAddrs rs defaultIndexers <$ e)
    tglE' <- fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "button button-outline m-0" $ if b then NSSClose else NSSAddUrl
    pure (refrE', tglE', e)
  pure ()

renderActive :: MonadFrontBase t m
  => NamedSockAddr
  -> Event t ()
  -> (Maybe (IndexerConnection t))
  -> m ()
renderActive nsa refrE mconn = mdo
  tglD <- holdDyn False tglE
  tglE <- lineOption $ do
    tglE' <- divClass "network-name" $ do
      let offclass = [("class", "mt-a mb-a indexer-offline")]
      let onclass = [("class", "mt-a mb-a indexer-online")]
      let maybe' m n j = maybe n j m
      let clsD = maybe' mconn (pure offclass) $ \con -> ffor (indexConIsUp con) $ \up -> if up then onclass else offclass
      elDynAttr "span" clsD $ elClass "i" "fas fa-circle" $ pure ()
      divClass "mt-a mb-a network-name-txt" $ text $ namedAddrName nsa
      fmap switchDyn $ widgetHoldDyn $ ffor tglD $ \b ->
        fmap (not b <$) $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a" $ if b then NSSClose else NSSEdit
    case mconn of
      Nothing -> descrOption NSSOffline
      Just conn -> do
        latD <- indexerConnPingerWidget conn refrE
        let heightD = fmap (M.lookup BTC) $ indexerConHeight conn
        descrOptionDyn $ NSSLatency <$> latD
        descrOptionDyn $ (maybe NSSNoHeight NSSIndexerHeight) <$> heightD
    pure tglE'
  void $ widgetHoldDyn $ ffor tglD $ \b -> if not b
    then pure ()
    else divClass "network-wrapper mt-2" $ do
      void $ deactivateURL . (nsa <$) =<< buttonClass "button button-outline mt-1 ml-1" NSSDisable
      void $ forgetURL . (nsa <$) =<< buttonClass "button button-outline mt-1 ml-1" NSSForget

inactivePageWidget :: forall t m . MonadFrontBase t m => m ()
inactivePageWidget = mdo
  addrsD <- externalRefDynamic =<< getInactiveAddrsRef
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  hideE <- deactivateURL =<< addUrlWidget showD
  let addrsMapD = (M.fromList . fmap (,()) . S.toList) <$> addrsD
  void $ listWithKey addrsMapD $ \addr _ -> renderInactive pingAllE addr
  (pingAllE, tglE) <- divClass "network-wrapper mt-1" $ divClass "net-btns-2" $ do
    pingAllE' <- buttonClass "button button-outline m-0" NSSPingAll
    tglE' <- fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "m-0 button button-outline" $ if b then NSSClose else NSSAddUrl
    pure (pingAllE', tglE')
  pure ()

renderInactive :: MonadFrontBase t m => Event t () -> NamedSockAddr -> m ()
renderInactive initPingE nsa = mdo
  sel <- getIndexReqSelector
  tglD <- holdDyn False tglE
  (fstPingE, refrE) <- headTailE $ leftmost [initPingE, pingE]
  tglE <- fmap switchDyn $ lineOption $ widgetHold (startingWidget tglD) $ ffor fstPingE $ const $ do
      let reqE = select sel $ Const2 (namedAddrSock nsa)
      conn <- initIndexerConnection nsa reqE
      pingD <- indexerConnPingerWidget conn refrE
      fmap switchDyn $ widgetHoldDyn $ ffor pingD $ \p -> do
        tglE' <- divClass "network-name" $ do
          let cls = if p == 0 then "mt-a mb-a indexer-offline" else "mt-a mb-a indexer-online"
          elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
          divClass "mt-a mb-a network-name-txt" $ text $ namedAddrName nsa
          tglBtn tglD
        descrOption $ NSSLatency p
        pure tglE'
  pingE <- fmap switchDyn $ widgetHoldDyn $ ffor tglD $ \b -> if not b
    then pure never
    else divClass "network-wrapper mt-1" $ divClass "net-btns-3" $ do
      void $ activateURL . (nsa <$) =<< outlineButton NSSEnable
      pingE' <- outlineButton NSSPing
      void $ forgetURL . (nsa <$) =<< outlineButton NSSForget
      pure pingE'
  pure ()
  where
    tglBtn :: MonadFrontBase t m => Dynamic t Bool -> m (Event t Bool)
    tglBtn tglD = fmap switchDyn $ widgetHoldDyn $ ffor tglD $ \b ->
      fmap (not b <$) $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a" $
        if b then NSSClose else NSSEdit

    startingWidget tglD = do
      tglE <- divClass "network-name" $ do
        divClass "mt-a mb-a network-name-txt" $ text $ namedAddrName nsa
        tglBtn tglD
      descrOption NSSOffline
      pure tglE

navbarWidget :: MonadFrontBase t m => NavbarItem -> m (Dynamic t NavbarItem)
navbarWidget initItem = divClass "navbar" $ mdo
  selD <- holdDyn initItem selE
  selE <- fmap leftmost $ flip traverse [ActivePage, DisabledPage, ParametersPage] $ \i -> do
    let attrD = (\ai -> "navbar-item" <> if i == ai then " active" else "") <$> selD
    pure . (<$) i =<< spanButton attrD i
  pure selD

lineOption :: MonadFrontBase t m => m a -> m a
lineOption = divClass "network-wrapper mt-1"

descrOption :: (MonadFrontBase t m, LocalizedPrint a) => a -> m ()
descrOption = divClass "network-descr" . localizedText

descrOptionDyn :: (MonadFrontBase t m, LocalizedPrint a) => Dynamic t a -> m ()
descrOptionDyn v = getLanguage >>= \langD -> (>>) elBR (divClass "network-descr" $ dynText $ ffor2 langD v localizedShow)

elBR :: MonadFrontBase t m => m ()
elBR = el "br" blank
