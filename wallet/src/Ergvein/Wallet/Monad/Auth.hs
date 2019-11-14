{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Random.Class
import Control.Monad.Reader
import Data.Functor (void)
import Data.List (permutations)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text, unpack)
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Client.Impl
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Unauth
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings (Settings(..), storeSettings)
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Storage.Util
import Network.Haskoin.Address
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef

import qualified Data.IntMap.Strict as MI
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L

data Env t = Env {
  env'settings        :: !(ExternalRef t Settings)
, env'backEF          :: !(Event t (), IO ())
, env'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, env'langRef         :: !(ExternalRef t Language)
, env'authRef         :: !(ExternalRef t AuthInfo)
, env'logoutFire      :: !(IO ())
, env'storeDir        :: !Text
, env'alertsEF        :: (Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alert event and trigger
, env'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, env'logsNameSpaces  :: !(ExternalRef t [Text])
, env'uiChan          :: !(Chan (IO ()))
, env'passModalEF     :: !(Event t Int, Int -> IO ())
, env'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
, env'urls            :: !(ExternalRef t (S.Set Text))
, env'urlNum          :: !(ExternalRef t Int)
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir

instance MonadBaseConstr t m => MonadEgvLogger t (ErgveinM t m) where
  getLogsTrigger = asks env'logsTrigger
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = asks env'logsNameSpaces
  {-# INLINE getLogsNameSpacesRef #-}

instance MonadBaseConstr t m => MonadLocalized t (ErgveinM t m) where
  setLanguage lang = do
    langRef <- asks env'langRef
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks env'langRef
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks env'langRef
  {-# INLINE getLanguage #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives) => MonadFrontBase t (ErgveinM t m) where
  getSettings = readExternalRef =<< asks env'settings
  {-# INLINE getSettings #-}
  getLoadingWidgetTF = asks env'loading
  {-# INLINE getLoadingWidgetTF #-}
  toggleLoadingWidget reqE = do
    fire <- asks (snd . env'loading)
    langRef <- asks env'langRef
    performEvent_ $ ffor reqE $ \(b,lbl) -> liftIO $ do
      lang <- readExternalRef langRef
      fire (b,localizedShow lang lbl)
  {-# INLINE toggleLoadingWidget #-}
  loadingWidgetDyn reqD = do
    fire <- asks (snd . env'loading)
    langRef <- asks env'langRef
    performEvent_ $ ffor (updated reqD) $ \(b,lbl) -> liftIO $ do
      lang <- readExternalRef langRef
      fire (b,localizedShow lang lbl)
  {-# INLINE loadingWidgetDyn #-}
  getBackEventFire = asks env'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks env'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks env'langRef
  {-# INLINE getLangRef #-}
  isAuthorized = do
    authd <- getAuthInfoMaybe
    pure $ ffor authd $ \case
      Just _ -> True
      Nothing -> False
  {-# INLINE isAuthorized #-}
  getAuthInfoMaybe = (fmap . fmap) Just $ externalRefDynamic =<< asks env'authRef
  {-# INLINE getAuthInfoMaybe #-}
  setAuthInfo e = do
    authRef <- asks env'authRef
    fire <- asks env'logoutFire
    performEvent $ ffor e $ \case
      Nothing -> liftIO fire
      Just v -> writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}
  getPasswordModalEF = asks env'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks env'passSetEF
  {-# INLINE getPasswordSetEF #-}
  requestPasssword reqE = do
    idE <- performEvent $ (liftIO getRandom) <$ reqE
    idD <- holdDyn 0 idE
    (_, modalF) <- asks env'passModalEF
    (setE, _) <- asks env'passSetEF
    performEvent_ $ fmap (liftIO . modalF) idE
    pure $ attachWithMaybe (\i' (i,mp) -> if i == i' then mp else Nothing) (current idD) setE
  updateSettings setE = do
    settingsRef <- asks env'settings
    performEvent_ $ ffor setE $ \s -> do
      writeExternalRef settingsRef s
      storeSettings s
  {-# INLINE updateSettings #-}
  getSettingsRef = asks env'settings
  {-# INLINE getSettingsRef #-}

instance MonadBaseConstr t m => MonadAlertPoster t (ErgveinM t m) where
  postAlert e = do
    (_, fire) <- asks env'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . env'alertsEF)
  getAlertEventFire = asks env'alertsEF
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance MonadBaseConstr t m => MonadStorage t (ErgveinM t m) where
  getEncryptedWallet = fmap storage'wallet $ readExternalRef =<< asks env'authRef
  {-# INLINE getEncryptedWallet #-}
  getAddressByCurIx cur i = do
    currMap <- fmap storage'pubKeys $ readExternalRef =<< asks env'authRef
    let maddr = MI.lookup i =<< M.lookup cur currMap
    case maddr of
      Nothing -> fail "NOT IMPLEMENTED" -- TODO: generate new address here
      Just addr -> pure addr
  {-# INLINE getAddressByCurIx #-}
  getWalletName = fmap storage'walletName $ readExternalRef =<< asks env'authRef
  {-# INLINE getWalletName #-}

-- | Execute action under authorized context or return the given value as result
-- is user is not authorized. Each time the login info changes (user logs out or logs in)
-- the widget is updated.
liftAuth :: MonadFrontBase t m => m a -> ErgveinM t m a -> m (Dynamic t a)
liftAuth ma0 ma = mdo
  mauthD <- holdUniqDyn =<< getAuthInfoMaybe
  mauth0 <- sample . current $ mauthD
  (logoutE, logoutFire) <- newTriggerEvent
  let runAuthed auth = do
        backEF          <- getBackEventFire
        loading         <- getLoadingWidgetTF
        langRef         <- getLangRef
        authRef         <- newExternalRef auth
        storeDir        <- getStoreDir
        alertsEF        <- getAlertEventFire
        logsTrigger     <- getLogsTrigger
        logsNameSpaces  <- getLogsNameSpacesRef
        uiChan          <- getUiChan
        passModalEF     <- getPasswordModalEF
        passSetEF       <- getPasswordSetEF
        settingsRef     <- getSettingsRef
        settings        <- readExternalRef settingsRef
        urlsRef         <- newExternalRef . S.fromList . settingsDefUrls $ settings
        urlNumRef       <- newExternalRef . settingsDefUrlNum $ settings
        let infoE = externalEvent authRef
        a <- runReaderT ma $ Env
          settingsRef backEF loading langRef authRef (logoutFire ()) storeDir alertsEF
          logsTrigger logsNameSpaces uiChan passModalEF passSetEF urlsRef urlNumRef
        pure (a, infoE)
  let
    ma0e = (,never) <$> ma0
    ma0' = maybe ma0e runAuthed mauth0
    redrawE = leftmost [updated mauthD, Nothing <$ logoutE]
  dres :: Dynamic t (a, Event t AuthInfo) <- widgetHold ma0' $ ffor redrawE $ maybe ma0e runAuthed
  let authInfoE = switch . current . fmap snd $ dres
  _ <- setAuthInfo $ Just <$> authInfoE
  pure $ fst <$> dres

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma

instance MonadFrontBase t m => MonadClient t (ErgveinM t m) where
  setRequiredUrlNum numE = do
    numRef <- asks env'urlNum
    performEvent_ $ (writeExternalRef numRef) <$> numE
  getUrlList reqE = do
    numRef <- asks env'urlNum
    urlsRef <- asks env'urls
    performEvent $ ffor reqE $ const $ liftIO $ do
      n <- readExternalRef numRef
      urls <- fmap S.elems $ readExternalRef urlsRef
      let perml = product [1 .. (length urls)] - 1
      i <- getRandomR (0, perml)
      pure $ take n $ permutations urls !! i

  addUrls urlsE = do
    urlsRef <- asks env'urls
    performEvent_ $ ffor urlsE $ \urls ->
      modifyExternalRef urlsRef (\s -> (S.union (S.fromList urls) s, ()) )

  invalidateUrls urlsE = do
    urlsRef <- asks env'urls
    performEvent_ $ ffor urlsE $ \urls ->
      modifyExternalRef urlsRef (\s -> (S.difference s (S.fromList urls), ()) )

  getBalance reqE        = requesterImpl reqE getBalanceImpl'
  getTxHashHistory reqE  = requesterImpl reqE getTxHashHistoryImpl'
  getTxMerkleProof reqE  = requesterImpl reqE getTxMerkleProofImpl'
  getTxHexView reqE      = requesterImpl reqE getTxHexViewImpl'
  getTxFeeHistogram reqE = requesterImpl reqE getTxFeeHistogramImpl'
  txBroadcast reqE       = requesterImpl reqE txBroadcastImpl'

data RequestMessages
  = RMSLoading Int Int
  | RMSError
  | RMSEmpty
  | RMSValidationError
  | RMSDone
  deriving (Eq)

instance LocalizedPrint RequestMessages where
  localizedShow l v = case l of
    English -> case v of
      RMSLoading i n      -> "Loading: " <> showt i <> " of " <> showt n
      RMSError            -> "A request has failed"
      RMSEmpty            -> "Results are empty"
      RMSValidationError  -> "Validation error: inconsistent results"
      RMSDone             -> "Done!"
    Russian -> case v of
      RMSLoading i n      -> "Запрашиваю. " <> showt i <> " из " <> showt n <> " ответили."
      RMSError            -> "Один из запросов не удался"
      RMSEmpty            -> "Результатов нет"
      RMSValidationError  -> "Ошибка: противоречивые ответы"
      RMSDone             -> "Готово!"

-- | Implements request logic:
-- Request from n nodes and check if results are equal.
-- TODO: Add more sophisticated validation
requesterImpl :: (MonadFrontBase t m, LocalizedPrint e, Eq a)
  => Event t b                                                              -- Request event
  -> (Dynamic t Text -> Event t b -> ErgveinM t m (Event t (Either e a)))   -- Request function
  -> ErgveinM t m (Event t a)                                               -- Result
requesterImpl reqE endpoint = do
  reqD  <- holdDyn Nothing $ Just <$> reqE        -- Hold request value for later
  uss   <- readExternalRef =<< asks env'urls      -- Set of urls
  n     <- readExternalRef =<< asks env'urlNum    -- Required number of confirmations
  urls  <- getRandUrls n uss                      -- Initial list of urls to query

  -- | Get a response event eresE :: Event t (Either e a) and split into failure and success events
  eresE <- fmap leftmost $ traverse (\u-> endpoint (pure u) reqE) urls
  let failE = fforMaybe eresE $ \case
        Left err -> Just err
        _ -> Nothing
      succE = fforMaybe eresE $ \case
        Right res -> Just res
        _ -> Nothing

  rec   -- If a request fails then get a new url and try again
    let totalFailE = leftmost [failE, extraFailE]
    -- | Get a new url and store it in Dynamic
    extraUrlD <- holdDyn "" =<< performEvent ((getExtraUrl uss) <$ totalFailE)
    -- | Wait a bit after totalFailE before firing another request, so that extraUrlD holds new url
    rereqE    <- delay 0.01 $ fmapMaybe id $ current reqD `tag` totalFailE
    -- | Get extra results, split, feed extraFailE back into totalFailE
    extraResE <- endpoint extraUrlD rereqE
    let extraFailE = fforMaybe extraResE $ \case
          Left err -> Just err
          _ -> Nothing
        extraSuccE = fforMaybe extraResE $ \case
          Right res -> Just res
          _ -> Nothing

  -- | Collect successful results
  resD <- foldDyn (:) [] $ leftmost [succE, extraSuccE]
  -- | When there is enough result, run validation and fire the final event away
  resE <- handleDangerMsg $ fforMaybe (updated resD) $ \rs -> if length rs >= n then Just (validateRes rs) else Nothing

  -- | Handle messages for loading display
  toggleLoadingWidget $ ffor reqE           $ \_        -> (True , RMSLoading 0 n)
  toggleLoadingWidget $ ffor totalFailE     $ \err      -> (True , RMSError)
  toggleLoadingWidget $ ffor (updated resD) $ \rs       -> (True , RMSLoading (length rs) n)
  toggleLoadingWidget $ ffor resE           $ \_        -> (False, RMSDone)
  delay 0.1 resE
  where
    validateRes :: Eq a => [a] -> Either RequestMessages a
    validateRes rs = case L.nub rs of
      []    -> Left RMSEmpty
      x:[]  -> Right x
      _     -> Left RMSValidationError

    getExtraUrl :: MonadIO m => S.Set Text -> m Text
    getExtraUrl uss = do
      i   <- liftIO $ getRandomR (0, S.size uss - 1)
      pure $ S.elems uss !! i

    getRandUrls :: MonadIO m => Int -> S.Set Text -> m [Text]
    getRandUrls n uss = do
      let us = S.elems uss
          perml = product [1 .. (length us)] - 1
      i <- liftIO $ getRandomR (0, perml)
      pure $ take n $ permutations us !! i
