{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Settings.Network
  (
    networkSettingsPage
  , networkSettingsPageUnauth
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Function
import Data.Functor.Misc (Const2(..))
import Data.List
import Data.Maybe
import Data.Maybe (isJust)
import Data.Ord
import Network.Socket
import Reflex.Dom
import Reflex.ExternalRef
import Text.Read

import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Elements.Toggle
import Ergvein.Wallet.Indexer.Socket
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T

data NavbarItem = ActivePage | ParametersPage
  deriving (Eq)

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      ActivePage      -> "Active indexers"
      ParametersPage  -> "Network parameters"
    Russian -> case v of
      ActivePage      -> "Используемые индексеры"
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
      ParametersPage  -> parametersPageWidget

networkSettingsPageUnauth :: MonadFrontBase t m => m ()
networkSettingsPageUnauth = wrapperSimple False $ do
  navD <- navbarWidget ActivePage
  void $ widgetHoldDyn $ ffor navD $ \case
    ActivePage      -> activePageWidget
    ParametersPage  -> parametersPageWidget

parametersPageWidget :: MonadFrontBase t m => m ()
parametersPageWidget = mdo
  setD <- getSettingsD
  valsD <- fmap join $
    widgetHoldDyn $ ffor setD $ \Settings{..} -> do
      let dt0 :: Double = realToFrac _settingsReqTimeout
      dtD <- fmap2 realToFrac $ textFieldValidated NSSReqTimeout dt0 $
        maybe (Left [PPENDT]) Right . readMaybe . T.unpack
      actNumD <- textFieldValidated NSSActUrlNum _settingsActUrlNum $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      rminD <- textFieldValidated NSSReqNumMin (fst _settingsReqUrlNum) $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      rmaxD <- textFieldValidated NSSReqNumMax (snd _settingsReqUrlNum) $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      pure $ (,,,) <$> dtD <*> actNumD <*> rminD <*> rmaxD
  divClass "net-btns-2" $ do
    saveE <- buttonClass "button button-outline" NSSSave
    defE <- buttonClass "button button-outline" NSSRestoreDef
    updE <- updateSettings $ flip pushAlways defE $ const $ do
      stngs <- sample $ current setD
      pure $ stngs {
            _settingsReqTimeout = defaultIndexerTimeout
          , _settingsActUrlNum  = defaultActUrlNum
        }
    updE' <- updateSettings $ flip pushAlways saveE $ const $ do
      stngs <- sample $ current setD
      (dt, actNum, rmin, rmax) <- sample $ current valsD
      pure $ stngs {
            _settingsReqTimeout = dt
          , _settingsReqUrlNum  = (rmin, rmax)
          , _settingsActUrlNum  = actNum
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
  let sortf a b = on (comparing Down) (_peerInfoIsPinned . snd) a b 
               <> on (comparing Down) (_peerInfoIsActive . snd) a b
  connsD  <- externalRefDynamic =<< getActiveConnsRef
  settD <- getSettingsD
  addrsD  <- holdUniqDyn $ _settingsAddrs <$> settD
  discoveryD  <- holdUniqDyn $ _settingsDiscoveryEnabled <$> settD
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  let valsD = (,) <$> connsD <*> addrsD
  
  dE <- updated <$> toggler NSSToggleDiscovery discoveryD 
  setDiscovery dE
  void $ widgetHoldDyn $ ffor valsD $ \(conmap, urls) -> do
    let sorted = sortBy sortf $ M.toList urls
    flip traverse sorted $ \(sa, nfo) -> renderActive sa nfo refrE $ M.lookup sa conmap
  hideE <- addManual =<< (fmap namedAddrName) <$> addUrlWidget showD
  (refrE, tglE) <- divClass "network-wrapper mt-3" $ divClass "net-btns-3" $ do
    refrE' <- buttonClass "button button-outline m-0" NSSRefresh
    tglE' <- fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "button button-outline m-0" $ if b then NSSPin else NSSAddUrl
    pure (refrE', tglE')
  pure ()

renderActive :: forall t m . MonadFrontBase t m
  => Text
  -> PeerInfo
  -> Event t ()
  -> (Maybe (IndexerConnection t))
  -> m ()
renderActive nodeAddress nodeInfo refreshE nodeConnection = mdo
  isNodeActiveD <- holdDyn (_peerInfoIsActive nodeInfo) nodeActivationE 
  let actBtn = fmap switchDyn $ widgetHoldDyn $ ffor isNodeActiveD $ \isActive -> fmap (not isActive <$)
        $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a"
        $ if isActive then NSSStop else NSSStart

  (nodeActivationE, deletionE) <- divClass "network-wrapper mt-3" $ do
    (nodeActivationE, deletionE) <- divClass "network-name" $ do
      case nodeConnection of
        Nothing -> elAttr "span" offclass $ elClass "i" "fas fa-circle" $ pure ()
        Just connection -> do
          let nodeHeightD = nodeHeight connection
          nodeStatusClassD <- nodeStatusClass connection nodeHeightD
          elDynAttr "span" nodeStatusClassD $ elClass "i" "fas fa-circle" $ pure ()
      divClass "mt-a mb-a network-name-txt" $ text $ nodeAddress
      when (_peerInfoIsPinned nodeInfo) $ text "PINNED"
      nodeActivationE <- actBtn
      deletionE <- buttonClass "button button-outline m-0" NSSRefresh
      pure (nodeActivationE, deletionE) 
    case nodeConnection of
      Nothing -> do
        descrOption NSSOffline
      Just connection -> do
        let nodeHeightD = nodeHeight connection
        latencyD <- indexerConnPingerWidget connection refreshE
        descrOptionDyn $ NSSLatency <$> latencyD
        descrOptionDyn $ (maybe NSSNoHeight NSSIndexerHeight) <$> nodeHeightD
    pure (nodeActivationE, deletionE)
  
  setAddrActive $ (nodeAddress,) <$> nodeActivationE
  void $ deleteAddr $ nodeAddress <$ deletionE
  where
    offclass    = [("class", "mb-a mt-a indexer-offline")]
    onclass     = [("class", "mb-a mt-a indexer-online")]
    unsyncClass = [("class", "mb-a mt-a indexer-unsync")]

    nodeHeight :: IndexerConnection t -> Dynamic t (Maybe BlockHeight)
    nodeHeight = fmap (M.lookup BTC) . indexerConHeight

    nodeStatusClass ::  MonadFrontBase t m => IndexerConnection t -> Dynamic t (Maybe BlockHeight) -> m (Dynamic t (M.Map Text Text))
    nodeStatusClass nodeConnection nodeHeightD = do
      let clsUnauthD = ffor (indexConIsUp nodeConnection) $ \up -> if up then onclass else offclass
      fmap join $ liftAuth (pure clsUnauthD) $ do
        walletHeightD <- getCurrentHeight BTC
        pure $ do
          nodeHeight <- nodeHeightD
          walletHeight <- fmap (Just . fromIntegral) walletHeightD
          isConnected <- indexConIsUp nodeConnection
          pure $ case (nodeHeight, walletHeight) of 
            (Just nodeH, Just walletH) | isConnected && nodeH >= pred walletH -> onclass
            _                          | isConnected                          -> unsyncClass
            otherwise                                                         -> offclass

visibilityClass :: Text -> Bool -> Text
visibilityClass classes True = classes <> " none"
visibilityClass classes False = classes

navbarWidget :: MonadFrontBase t m => NavbarItem -> m (Dynamic t NavbarItem)
navbarWidget initItem = divClass "navbar-2-cols" $ mdo
  selD <- holdDyn initItem selE
  selE <- fmap leftmost $ flip traverse [ActivePage] $ \i -> do
    let attrD = (\ai -> "navbar-item" <> if i == ai then " active" else "") <$> selD
    pure . (<$) i =<< spanButton attrD i
  pure selD

descrOption :: (MonadFrontBase t m, LocalizedPrint a) => a -> m ()
descrOption = divClass "network-descr" . localizedText

descrOptionDyn :: (MonadFrontBase t m, LocalizedPrint a) => Dynamic t a -> m ()
descrOptionDyn v = getLanguage >>= \langD -> divClass "network-descr" $ dynText $ ffor2 langD v localizedShow
