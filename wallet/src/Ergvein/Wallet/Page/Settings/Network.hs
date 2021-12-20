{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Settings.Network
  (
    networkSettingsPage
  , networkSettingsPageUnauth
  ) where

import Control.Lens
import Data.Functor.Misc (Const2(..))
import Data.Traversable (for)
import Reflex.Dom
import Reflex.ExternalRef
import Text.Read

import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Sepulcas.Alert
import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S

data NavbarItem
  = ActiveIndexers
  | InactiveIndexers
  deriving Eq

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      ActiveIndexers    -> "Active indexers"
      InactiveIndexers  -> "Reserved indexers"
    Russian -> case v of
      ActiveIndexers     -> "Используемые индексаторы"
      InactiveIndexers   -> "Неактивные индексаторы"

navbarWidget :: MonadFrontBase t m => NavbarItem -> m (Dynamic t NavbarItem)
navbarWidget initItem = divClass "navbar-2-cols" $ mdo
  selD <- holdDyn initItem selE
  selE <- fmap leftmost $ for [ActiveIndexers, InactiveIndexers] $ \i -> do
    let attrD = (\ai -> "navbar-item" <> if i == ai then " active" else "") <$> selD
    (<$) i <$> spanButton attrD i
  pure selD

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
    navD <- navbarWidget ActiveIndexers
    void $ networkHoldDyn $ ffor navD $ \case
      ActiveIndexers   -> activePageWidget
      InactiveIndexers -> inactivePageWidget

networkSettingsPageUnauth :: MonadFrontBase t m => m ()
networkSettingsPageUnauth = wrapperSimple False $ do
  navD <- navbarWidget ActiveIndexers
  void $ networkHoldDyn $ ffor navD $ \case
    ActiveIndexers   -> activePageWidget
    InactiveIndexers -> inactivePageWidget

addUrlWidget :: forall t m . MonadFrontBase t m => Dynamic t Bool -> m (Event t ErgveinNodeAddr)
addUrlWidget showD = fmap switchDyn $ networkHoldDyn $ ffor showD $ \b -> if not b then pure never else do
  murlE <- divClass "mt-3" $ do
    textD <- fmap _inputElement_value $ inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
    goE <- outlineButton NSSAddUrl
    rs <- mkResolvSeed
    performFork $ ffor goE $ const $ do
      t <- sampleDyn textD
      resolveAddr rs defIndexerPort t
  void $ networkHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors" $ localizedText NPSParseError
    _ -> pure ()
  pure $ fmapMaybe (namedAddrName <$>) murlE

activePageWidget :: forall t m . MonadFrontBase t m => m ()
activePageWidget = mdo
  connsD  <- externalRefDynamic =<< getActiveConnsRef
  addrsD  <- (fmap . fmap) S.toList $ externalRefDynamic =<< getActiveAddrsRef
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  let valsD = (,) <$> connsD <*> addrsD
  void $ networkHoldDyn $ ffor valsD $ \(conmap, urls) ->
    for urls $ \sa -> renderActive sa refrE $ M.lookup sa conmap
  hideE <- activateURL =<< addUrlWidget showD
  (refrE, tglE) <- divClass "network-wrapper mt-3" $ divClass "net-btns-3" $ do
    refrE' <- buttonClass "button button-outline m-0" NSSRefresh
    restoreE <- buttonClass "button button-outline m-0" NSSRestoreUrls
    restoreNetwork restoreE
    tglE' <- fmap switchDyn $ networkHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "button button-outline m-0" $ if b then NSSClose else NSSAddUrl
    pure (refrE', tglE')
  pure ()

renderActive :: MonadFrontBase t m
  => ErgveinNodeAddr
  -> Event t ()
  -> Maybe (IndexerConnection t)
  -> m ()
renderActive nsa refrE mconn = mdo
  tglD <- holdDyn False tglE
  let editBtn = fmap switchDyn $ networkHoldDyn $ ffor tglD $ \b -> fmap (not b <$)
        $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a"
          $ if b then NSSClose else NSSEdit

  tglE <- divClass "network-wrapper mt-3" $ case mconn of
    Nothing -> do
      tglE' <- divClass "network-name" $ do
        elAttr "span" offclass $ elClass "i" "fas fa-circle" $ pure ()
        divClass "mt-a mb-a network-name-txt" $ text nsa
        editBtn
      stD <- indexerLastStatus nsa
      _ <- networkHoldDyn $ ffor stD $ \case
        Just (IndexerWrongVersion v) -> descrOption $ NSSWrongVersion v
        Just IndexerMissingCurrencies -> descrOption NSSMissingCurrencies
        Just IndexerNotSynced -> descrOption NSSNotSynced
        _ -> descrOption NSSOffline
      pure tglE'
    Just conn -> do
      let clsUnauthD = ffor (indexConIsUp conn) $ \up -> if up then onclass else offclass
      let isUpD = ffor (indexConIsUp conn) id
      let indexerHeightD = M.lookup BTC <$> indexConHeight conn
      clsD <- fmap join $ liftAuth (pure clsUnauthD) $ do
        hD <- getCurrentHeight BTC
        pure $ do
          h <- indexerHeightD
          h' <- fmap (Just . fromIntegral) hD
          up <- indexConIsUp conn
          let synced = h >= h' || Just 1 == ((-) <$> h' <*> h)
          pure $ if up
            then if synced then onclass else unsyncClass
            else offclass
      tglE' <- divClass "network-name" $ do
        elDynAttr "span" clsD $ elClass "i" "fas fa-circle" $ pure ()
        divClass "mt-a mb-a network-name-txt" $ text nsa
        editBtn
      latD <- indexerConnPingerWidget conn refrE
      _ <- networkHoldDyn $ ffor isUpD $ \up -> if not up
          then descrOption NSSOffline
          else do
            descrOptionDyn $ NSSLatency <$> latD
            let unauthHeight = descrOptionDyn $ maybe NSSNoHeight NSSIndexerHeight <$> indexerHeightD
            liftAuth unauthHeight $ do
              btcHeightD <- getCurrentHeight BTC
              descrOptionDyn $ do
                mh <- indexerHeightD
                hb <- btcHeightD
                pure $ maybe NSSNoHeight (NSSIndexerHeightAuth hb) mh
            descrOptionDyn $ NPSIndexerVersion <$> indexConIndexerVersion conn
      pure tglE'

  void $ networkHoldDyn $ ffor tglD $ \b -> if not b
    then pure ()
    else divClass "network-wrapper mt-2" $ do
      void $ deactivateURL . (nsa <$) =<< buttonClass "button button-outline mt-1 ml-1" NSSDisable
      void $ forgetURL . (nsa <$) =<< buttonClass "button button-outline mt-1 ml-1" NSSForget
  where
    offclass    = [("class", "mb-a mt-a indexer-offline")]
    onclass     = [("class", "mb-a mt-a indexer-online")]
    unsyncClass = [("class", "mb-a mt-a indexer-unsync")]

inactivePageWidget :: forall t m . MonadFrontBase t m => m ()
inactivePageWidget = mdo
  addrsD <- externalRefDynamic =<< getInactiveAddrsRef
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  hideE <- deactivateURL =<< addUrlWidget showD
  let addrsMapD = M.fromList . fmap (,()) . S.toList <$> addrsD
  void $ listWithKey addrsMapD $ \addr _ -> renderInactive pingAllE addr
  (pingAllE, tglE) <- divClass "network-wrapper mt-1" $ divClass "net-btns-2" $ do
    pingAllE' <- buttonClass "button button-outline m-0" NSSPingAll
    tglE' <- fmap switchDyn $ networkHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "m-0 button button-outline" $ if b then NSSClose else NSSAddUrl
    pure (pingAllE', tglE')
  pure ()

renderInactive :: MonadFrontBase t m => Event t () -> ErgveinNodeAddr -> m ()
renderInactive initPingE nsa = mdo
  seed <- mkResolvSeed
  sel <- getIndexReqSelector
  tglD <- holdDyn False tglE
  (fstPingE, refrE) <- headTailE $ leftmost [initPingE, pingE]
  tglE <- fmap switchDyn $ divClass "network-wrapper mt-1" $ networkHold (startingWidget tglD) $ ffor fstPingE $ const $ do
    mAddr <- fmap namedAddrSock <$> resolveAddr seed defIndexerPort nsa
    case mAddr of
      Just addr -> do
        let reqE = select sel $ Const2 nsa
        conn <- initIndexerConnection nsa addr reqE
        pingD <- indexerConnPingerWidget conn refrE
        fmap switchDyn $ networkHoldDyn $ ffor pingD $ \p -> do
          tglE' <- divClass "network-name" $ do
            let cls = if p == 0 then "mt-a mb-a indexer-offline" else "mt-a mb-a indexer-online"
            elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
            divClass "mt-a mb-a network-name-txt" $ text nsa
            tglBtn tglD
          descrOption $ NSSLatency p
          pure tglE'
      _ -> pure never
  pingE <- fmap switchDyn $ networkHoldDyn $ ffor tglD $ \b -> if not b
    then pure never
    else divClass "network-wrapper mt-1" $ divClass "net-btns-3" $ do
      void $ activateURL . (nsa <$) =<< outlineButton NSSEnable
      pingE' <- outlineButton NSSPing
      void $ forgetURL . (nsa <$) =<< outlineButton NSSForget
      pure pingE'
  pure ()
  where
    tglBtn :: MonadFrontBase t m => Dynamic t Bool -> m (Event t Bool)
    tglBtn tglD = fmap switchDyn $ networkHoldDyn $ ffor tglD $ \b ->
      fmap (not b <$) $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a" $
        if b then NSSClose else NSSEdit

    startingWidget tglD = do
      tglE <- divClass "network-name" $ do
        divClass "mt-a mb-a network-name-txt" $ text nsa
        tglBtn tglD
      descrOption NSSOffline
      pure tglE

descrOption :: (MonadFrontBase t m, LocalizedPrint a) => a -> m ()
descrOption = divClass "network-descr" . localizedText

descrOptionDyn :: (MonadFrontBase t m, LocalizedPrint a) => Dynamic t a -> m ()
descrOptionDyn v = getLanguage >>= \langD -> divClass "network-descr" $ dynText $ ffor2 langD v localizedShow
