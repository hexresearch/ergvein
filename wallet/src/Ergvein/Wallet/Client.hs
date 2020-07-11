{-# LANGUAGE RecursiveDo #-}
module Ergvein.Wallet.Client
  ( getHeight
  , getHeightSolo
  , getHeightRandom
  , getBlockFilters
  , getBlockFiltersSolo
  , getBlockFiltersRandom
  , getFeeEstimatesRandom
  , ClientErr(..)
  , module Ergvein.Index.API.Types
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable
import Data.List (sortOn)
import Data.Maybe
import Data.Traversable
import Network.HTTP.Client (Manager)
import Reflex.ExternalRef
import Servant.Client

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as MI
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S

import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Localization.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native

getHeight :: (MonadFrontBase t m, MonadClient t m) => Event t HeightRequest -> m (Event t (Either ClientErr HeightResponse))
getHeight = requester False meanHeight getHeightEndpoint

getHeightSolo :: (MonadFrontBase t m, MonadClient t m) => Event t (BaseUrl, HeightRequest) -> m (Event t (Either ClientErr HeightResponse))
getHeightSolo = requestSolo getHeightEndpoint

getHeightRandom :: (MonadFrontBase t m, MonadClient t m) => Event t HeightRequest -> m (Event t (Either ClientErr HeightResponse))
getHeightRandom = requestSoloRandom getHeightEndpoint

meanHeight :: [HeightResponse] -> Either ClientMessage HeightResponse
meanHeight [] = Left CMSEmpty
meanHeight [x] = Right x
meanHeight xs = Right $ head $ drop (length xs `div` 2) $ sortOn heightRespHeight xs

getBlockFilters :: (MonadFrontBase t m, MonadClient t m) => Event t BlockFiltersRequest -> m (Event t (Either ClientErr BlockFiltersResponse))
getBlockFilters = requesterEq getBlockFiltersEndpoint

getBlockFiltersSolo :: (MonadFrontBase t m, MonadClient t m) => Event t (BaseUrl, BlockFiltersRequest) -> m (Event t (Either ClientErr BlockFiltersResponse))
getBlockFiltersSolo = requestSolo getBlockFiltersEndpoint

getBlockFiltersRandom :: (MonadFrontBase t m, MonadClient t m) => Event t BlockFiltersRequest -> m (Event t (Either ClientErr BlockFiltersResponse))
getBlockFiltersRandom = requestSoloRandom getBlockFiltersEndpoint

-- | TODO: Request from multiple indexers and return average fees
getFeeEstimatesRandom :: (MonadFrontBase t m, MonadClient t m) => Event t [Currency] -> m (Event t (Either ClientErr IndexFeesResp))
getFeeEstimatesRandom = requestSoloRandom getFeeEstimatesEndpoint


data ResAct = ResActCheck | ResActTimeout | ResActNoUrls

data ReqSignal a b = ReqFin a | ReqFailTimeout | ReqFailUrls | ReqTimeout b [a] (S.Set BaseUrl)

validateEq :: Eq a => [a] -> Either ClientMessage a
validateEq rs = case L.nub rs of
  []    -> Left CMSEmpty
  x:[]  -> Right x
  _     -> Left CMSValidationError

requestSoloRandom :: (MonadFrontBase t m, MonadClient t m, Eq a, Show a, Show b)
  => (BaseUrl -> b -> ReaderT Manager IO (Either e a))
  -> Event t b
  -> m (Event t (Either ClientErr a))
requestSoloRandom endpoint reqE = do
  urls  <- fmap M.keys . readExternalRef =<< getActiveUrlsRef
  i <- liftIO $ randomRIO (0, length urls - 1)
  requestSolo endpoint $ (urls!!i, ) <$> reqE

requestSolo :: (MonadFrontBase t m, MonadClient t m, Eq a, Show a, Show b)
  => (BaseUrl -> b -> ReaderT Manager IO (Either e a))
  -> Event t (BaseUrl, b)
  -> m (Event t (Either ClientErr a))
requestSolo endpoint reqE = do
  mng <- getClientManager
  resE <- performFork $ ffor reqE $ \(u, req) -> liftIO $ runReaderT (endpoint u req) mng
  pure $ either (const $ Left ClientErrTimeOut) Right <$> resE

-- Implements request logic:
-- Request from n nodes and check if results are equal.
requesterEq :: (MonadFrontBase t m, MonadClient t m, Eq a, Show a, Show b)
  => (BaseUrl -> b -> ReaderT Manager IO (Either e a))    -- ^ Request function
  -> Event t b                                            -- ^ Request event
  -> m (Event t (Either ClientErr a))                     -- ^ Result
requesterEq = requester False validateEq

-- Implements request logic:
-- Request from n nodes and check if results are equal.
requester :: (MonadFrontBase t m, MonadClient t m, Eq a, Show a, Show b)
  => Bool                                                 -- ^ Show loading widget or not
  -> ([a] -> Either ClientMessage a)                      -- ^ Validation of inputs
  -> (BaseUrl -> b -> ReaderT Manager IO (Either e a))    -- ^ Request function
  -> Event t b                                            -- ^ Request event
  -> m (Event t (Either ClientErr a))                     -- ^ Result
requester showLoad validateRes endpoint reqE = mdo
  uss  <- fmap M.keysSet . readExternalRef =<< getActiveUrlsRef
  let initReqE = ffor reqE (\req -> Just (req, [], uss))
  drawE <- delay 0.1 $ leftmost [initReqE, redrawE]
  respE <- fmap (switch . current) $ widgetHold (pure never) $ ffor drawE $ \case
    Just (req, rs, urls) -> requesterBody showLoad validateRes urls endpoint rs req
    _ -> pure never
  let redrawE = ffor respE $ \case
        ReqTimeout req [] uss -> Just (req, [], uss)
        _ -> Nothing

  pure $ fforMaybe respE $ \case
    ReqFin res        -> Just $ Right res
    ReqFailTimeout    -> Just $ Left ClientErrTimeOut
    ReqFailUrls       -> Just $ Left ClientErrNoUrls
    ReqTimeout {}     -> Nothing

requesterBody :: forall t m e a b . ((MonadFrontBase t m, MonadClient t m), Show a)
  => Bool                                                 -- ^ Show loading widget or not
  -> ([a] -> Either ClientMessage a)                      -- ^ Validation of inputs
  -> S.Set BaseUrl
  -> (BaseUrl -> b -> ReaderT Manager IO (Either e a))
  -> [a]
  -> b
  -> m (Event t (ReqSignal a b))
requesterBody showLoad validateRes uss endpoint initRes req = do
  buildE        <- getPostBuild
  (minN, maxN)  <- readExternalRef =<< getRequiredUrlNumRef -- Required number of confirmations
  dt            <- readExternalRef =<< getRequestTimeoutRef -- Get a timeout
  timeoutMsgE   <- delay dt buildE                          -- When to show a timeout message
  timeoutE      <- delay 1 timeoutMsgE                      -- When to actually signal a timeout
  urls          <- getRandUrls maxN uss                     -- Starting urls. Asked concurrently
  mng           <- getClientManager                         -- Client manger for requests
  resCountRef   <- liftIO $ newTVarIO $ S.size uss          -- A counter of "visited" urls. Counts down till 0

  -- Request all staring urls concurrently and give back events and triggers
  (ereses, triggers)  <- fmap unzip $ for (zip [1..] urls) $ \(i, u) -> do
    (ev, fire) <- newTriggerEvent
    void $ liftIO $ forkOnOther $ do
      fire =<< (pure . pure . (i,)) =<< runReaderT (endpoint u req) mng
    pure (ev, (i,fire))

  let eresE :: Event t [(Int, Either e a)]            -- a response event. Carries ids and possble results
      fires :: IntMap ([(Int, Either e a)] -> IO ())  -- a map of all the triggers
      failE :: Event t [Int]                          -- falure event. Carries ids of failed requests
      succE :: Event t [a]                            -- success event. Carries results
      eresE = mconcat ereses
      fires = MI.fromList triggers
      failE = fforMaybe eresE $ \ies -> let
        ids = flip fmap ies $ \(i,eres) -> either (const $ Just i) (const Nothing) eres
        in nullToMaybe $ catMaybes ids
      succE = fforMaybe eresE $ \ies -> let
        ress = flip fmap ies $ \(i,eres) -> either (const Nothing) Just eres
        in nullToMaybe $ catMaybes ress

  -- Whenever a request has ended, bump the number of "used" urls
  -- If a request has failed, check if there are still unused urls
  -- If there are none -- send ResActNoUrls
  noMoreUrlE' <- fmap (fmapMaybe id) $ performEvent $ ffor eresE $ \ies -> do
    ress <- for ies $ \(i, eres) -> liftIO $ do
      c <- atomically $ modifyTVar' resCountRef (flip (-) 1) >> readTVar resCountRef
      pure $ either (const $ if c <= 1 then Just ResActNoUrls else Nothing) (const Nothing) eres
    pure $ listToMaybe $ catMaybes ress
  noMoreUrlE <- delay 0.5 noMoreUrlE'

  rec
    usedUrlsD   <- foldDyn (\us acc -> foldr S.insert acc $ fmap snd us) (S.fromList urls) extraUrlsE
    extraUrlsE  <- fmap (fmapMaybe id) $ performEvent $ ffor (current usedUrlsD `attach` failE) $ \(used, ies) -> do
      ress <- foldM (\(diff, acc) i -> if S.null diff then pure (diff, acc) else do
          u <- getExtraUrl diff
          pure (S.delete u diff, (i,u):acc)
          ) (S.difference uss used,[]) ies
      pure $ nullToMaybe $ snd ress

  -- Request from extra url. i is ised to pick a particular event trigger
  -- Otherwise, if performEvent returns an event with extra results,
  -- it could get messy if, while a forked request waits for a response,
  -- another request fails and fiers extraUrlE again, overriding control of the event
  -- At least this is a working theory
  performEvent_ $ ffor extraUrlsE $ \ius -> for_ ius $ \(i,url) -> whenJust (MI.lookup i fires) $
    \fire -> void $ liftIO $ forkOnOther $ fire =<< (pure . pure . (i,)) =<< runReaderT (endpoint url req) mng

  storeD  <- foldDyn (++) initRes succE
  checkE  <- delay 0.05 succE
  rec
    let actE = gate gateB $ leftmost [ResActTimeout <$ timeoutE, noMoreUrlE, ResActCheck <$ checkE]
    let preValidE = flip push actE $ \act -> do
          res   <- sample $ current storeD
          used  <- sample $ current usedUrlsD
          let l = length res
          let diff = S.difference uss used
          pure $ case act of
              ResActCheck   -> if l >= maxN then Just (Right res) else Nothing
              ResActTimeout -> Just $ if l >= minN
                then Right res
                else Left $ if S.null diff then ReqFailTimeout else ReqTimeout req res diff
              ResActNoUrls  -> Just $ if l >= minN then Right res else Left ReqFailUrls

    resE <- handleDangerMsg (ffor preValidE $ either Right (fmap ReqFin . validateRes))
    -- When a result has fired -- stop caring about the rest and block all subsequent acts
    gateB <- fmap current $ holdDyn True $ False <$ resE

  -- Handle messages for loading display
  when showLoad $ do
    toggleLoadingWidget $ (True , CMSTimeout) <$ timeoutMsgE
    toggleLoadingWidget $ (True , CMSError)   <$ failE
    toggleLoadingWidget $ ffor resE $ \case
      ReqTimeout{} -> (True, CMSRestarting)
      _ -> (False, CMSDone)
    toggleLoadingWidget =<< fmap ((True , CMSLoading 0 minN maxN) <$) getPostBuild
    toggleLoadingWidget $ ffor (updated storeD) $ \store -> (True , CMSLoading (length store) minN maxN)

  delay 0.1 resE
  where
    getExtraUrl :: MonadIO m => S.Set BaseUrl -> Performable m BaseUrl
    getExtraUrl uss = liftIO $ (S.elems uss !!) <$> getRandomR (0, length uss - 1)

    getRandUrls :: MonadIO m => Int -> S.Set BaseUrl -> m [BaseUrl]
    getRandUrls n usSet = liftIO $ do
      let uss = S.elems usSet
      let perml = product [1 .. (length uss)] - 1
      i <- getRandomR (0, perml)
      pure $ take n $ L.permutations uss !! i

    nullToMaybe :: [x] -> Maybe [x]
    nullToMaybe xs = case xs of
      [] -> Nothing
      _  -> Just xs

    whenJust :: Monad m' => Maybe v -> (v -> m' ()) -> m' ()
    whenJust mv f = case mv of
      Nothing -> pure ()
      Just v  -> f v
