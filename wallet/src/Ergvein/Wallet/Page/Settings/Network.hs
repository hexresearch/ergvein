{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Settings.Network
  (
    activeIndexersPage
  , activeIndexersPageUnauth
  , inactiveIndexersPage
  , inactiveIndexersPageUnauth
  ) where

import Control.Lens
import Data.Functor.Misc (Const2(..))
import Data.Traversable (for)
import Reflex.Dom
import Reflex.ExternalRef

import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements
import {-# SOURCE #-} Ergvein.Wallet.Navbar

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data ParametersParseErrors = PPENDT | PPEInt

instance LocalizedPrint ParametersParseErrors where
  localizedShow l v = case l of
    English -> case v of
      PPENDT -> "Failed to parse seconds"
      PPEInt -> "Failed to parse integer"
    Russian -> case v of
      PPENDT -> "Некорректное значение. Только дробные числа"
      PPEInt -> "Некорректное значение. Только целые числа"

activeIndexersPage :: MonadFront t m => m ()
activeIndexersPage = do
  title <- localized NSSTitle
  let thisWidget = Just $ pure activeIndexersPage
      navbar = networkPageNavbarWidget thisWidget NavbarActiveIndexers
  wrapperGeneric False title thisWidget (Just navbar) "flex-column" $ divClass "network-page" $ do
    activePageWidget

inactiveIndexersPage :: MonadFront t m => m ()
inactiveIndexersPage = do
  title <- localized NSSTitle
  let thisWidget = Just $ pure inactiveIndexersPage
      navbar = networkPageNavbarWidget thisWidget NavbarInactiveIndexers
  wrapperGeneric False title thisWidget (Just navbar) "flex-column" $ divClass "network-page" $ do
    inactivePageWidget

activeIndexersPageUnauth :: MonadFrontBase t m => m ()
activeIndexersPageUnauth = do
  let thisWidget = Just $ pure activeIndexersPageUnauth
      navbar = networkPageNavbarWidgetUnauth thisWidget NavbarActiveIndexers
  wrapperSimpleGeneric headerWidgetOnlyBackBtn "flex-column" False (Just navbar) $ divClass "network-page" $ do
    activePageWidget

inactiveIndexersPageUnauth :: MonadFrontBase t m => m ()
inactiveIndexersPageUnauth = do
  let thisWidget = Just $ pure inactiveIndexersPageUnauth
      navbar = networkPageNavbarWidgetUnauth thisWidget NavbarInactiveIndexers
  wrapperSimpleGeneric headerWidgetOnlyBackBtn "flex-column" False (Just navbar) $ divClass "network-page" $ do
    inactivePageWidget

addUrlWidget :: forall t m . MonadFrontBase t m => Dynamic t Bool -> m (Event t ErgveinNodeAddr)
addUrlWidget showD = fmap switchDyn $ networkHoldDyn $ ffor showD $ \b -> if not b then pure never else do
  murlE <- divClass "mb-1" $ do
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
  connsD <- externalRefDynamic =<< getActiveConnsRef
  addrsD <- (fmap . fmap) S.toList $ externalRefDynamic =<< getActiveAddrsRef
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  let valsD = (,) <$> connsD <*> addrsD
  void $ divClass "network-page-items mb-2" $
    networkHoldDyn $ ffor valsD $ \(conmap, urls) ->
      for urls $ \sa -> renderActive sa never $ M.lookup sa conmap
  (tglE, hideE) <- divClass "network-page-footer" $ do
    hideE' <- activateURL =<< addUrlWidget showD
    tglE'' <- divClass "net-btns-2" $ do
      tglE' <- fmap switchDyn $ networkHoldDyn $ ffor showD $ \b ->
        fmap (not b <$) $ buttonClass "button button-outline m-0" $ if b then NSSClose else NSSAddUrl
      restoreE <- buttonClass "button button-outline m-0" NSSRestoreUrls
      restoreNetwork restoreE
      pure tglE'
    pure (tglE'', hideE')
  pure ()

renderActive :: MonadFrontBase t m
  => ErgveinNodeAddr
  -> Event t ()
  -> Maybe (IndexerConnection t)
  -> m ()
renderActive nsa refrE mconn = mdo
  tglD <- holdDyn False tglE''
  let editBtn = fmap switchDyn $ networkHoldDyn $ ffor tglD $ \b -> fmap (not b <$)
        $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a"
          $ if b then NSSClose else NSSEdit
  tglE'' <- divClass "network-item" $ do
    tglE <- case mconn of
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
              void $ liftAuth unauthHeight $ do
                btcHeightD <- getCurrentHeight BTC
                descrOptionDyn $ do
                  mh <- indexerHeightD
                  hb <- btcHeightD
                  pure $ maybe NSSNoHeight (NSSIndexerHeightAuth hb) mh
              descrOptionDyn $ NPSIndexerVersion <$> indexConIndexerVersion conn
        pure tglE'
    
    void $ networkHoldDyn $ ffor tglD $ \b -> if not b
      then pure ()
      else divClass "net-btns-2 mt-1" $ do
        void $ deactivateURL . (nsa <$) =<< buttonClass "button button-outline m-0" NSSDisable
        void $ forgetURL . (nsa <$) =<< buttonClass "button button-outline m-0" NSSForget
    
    pure tglE
  pure ()
  where
    offclass    = [("class", "mb-a mt-a indexer-offline")]
    onclass     = [("class", "mb-a mt-a indexer-online")]
    unsyncClass = [("class", "mb-a mt-a indexer-unsync")]

inactivePageWidget :: forall t m . MonadFrontBase t m => m ()
inactivePageWidget = mdo
  addrsD <- externalRefDynamic =<< getInactiveAddrsRef
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  let addrsMapD = M.fromList . fmap (,()) . S.toList <$> addrsD
  divClass "network-page-items mb-2" $ do
    void $ listWithKey addrsMapD $ \addr _ -> renderInactive pingAllE addr
  (pingAllE, tglE, hideE) <- divClass "network-page-footer" $ do
    hideE' <- deactivateURL =<< addUrlWidget showD
    (pingAllE', tglE') <- divClass "net-btns-2" $ do
      tglE'' <- fmap switchDyn $ networkHoldDyn $ ffor showD $ \b ->
        fmap (not b <$) $ buttonClass "button button-outline m-0" $ if b then NSSClose else NSSAddUrl
      pingAllE'' <- buttonClass "button button-outline m-0" NSSPingAll
      pure (pingAllE'', tglE'')
    pure (pingAllE', tglE', hideE')
  pure ()

renderInactive :: MonadFrontBase t m => Event t () -> ErgveinNodeAddr -> m ()
renderInactive initPingE nsa = mdo
  seed <- mkResolvSeed
  sel <- getIndexReqSelector
  tglD <- holdDyn False tglE
  (fstPingE, refrE) <- headTailE $ leftmost [initPingE, pingE]
  (tglE, pingE) <- divClass "network-item" $ do
    tglE' <- fmap switchDyn $ networkHold (startingWidget tglD) $ ffor fstPingE $ const $ do
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
    pingE' <- fmap switchDyn $ networkHoldDyn $ ffor tglD $ \b -> if not b
      then pure never
      else divClass "net-btns-3 mt-1" $ do
        void $ activateURL . (nsa <$) =<< buttonClass "button button-outline m-0" NSSEnable
        void $ forgetURL . (nsa <$) =<< buttonClass "button button-outline m-0" NSSForget
        buttonClass "button button-outline m-0" NSSPing
    pure (tglE', pingE')
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
