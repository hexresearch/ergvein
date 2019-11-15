module Ergvein.Wallet.Client
  (
    getBalance
  , getTxHashHistory
  , getTxMerkleProof
  , getTxHexView
  , getTxFeeHistogram
  , txBroadcast
  ) where

import Control.Concurrent
import Control.Concurrent.Lifted hiding (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Random
import Reflex.ExternalRef
import Servant.Client

import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Front

import qualified Data.List as L
import qualified Data.Set as S

import Network.HTTP.Client (Manager)

getBalance :: MonadFrontBase t m => Event t BalanceRequest -> m (Event t BalanceResponse)
getBalance = requesterImpl getBalanceEndpoint

getTxHashHistory :: MonadFrontBase t m => Event t TxHashHistoryRequest -> m (Event t TxHashHistoryResponse)
getTxHashHistory = requesterImpl getTxHashHistoryEndpoint

getTxMerkleProof :: MonadFrontBase t m => Event t TxMerkleProofRequest -> m (Event t TxMerkleProofResponse)
getTxMerkleProof = requesterImpl getTxMerkleProofEndpoint

getTxHexView :: MonadFrontBase t m => Event t TxHexViewRequest -> m (Event t TxHexViewResponse)
getTxHexView = requesterImpl getTxHexViewEndpoint

getTxFeeHistogram :: MonadFrontBase t m => Event t TxFeeHistogramRequest -> m (Event t TxFeeHistogramResponse)
getTxFeeHistogram = requesterImpl getTxFeeHistogramEndpoint

txBroadcast :: MonadFrontBase t m => Event t TxBroadcastRequest -> m (Event t TxBroadcastResponse)
txBroadcast = requesterImpl txBroadcastEndpoint

data ClientMessages
  = CMSLoading Int Int Int
  | CMSError
  | CMSEmpty
  | CMSValidationError
  | CMSDone
  | CMSTimeout
  deriving (Eq)

instance LocalizedPrint ClientMessages where
  localizedShow l v = case l of
    English -> case v of
      CMSLoading i mi ma  -> "Loading: " <> showt i <> " of " <> showt ma <> " (" <> showt mi <> ")"
      CMSError            -> "A request has failed"
      CMSEmpty            -> "Results are empty"
      CMSValidationError  -> "Validation error: inconsistent results"
      CMSDone             -> "Done!"
      CMSTimeout          -> "Time out!"
    Russian -> case v of
      CMSLoading i mi ma  -> "Запрашиваю. " <> showt i <> " из " <> showt ma <> " (" <> showt mi <> ") ответили."
      CMSError            -> "Один из запросов не удался"
      CMSEmpty            -> "Результатов нет"
      CMSValidationError  -> "Ошибка: противоречивые ответы"
      CMSDone             -> "Готово!"
      CMSTimeout          -> "Время вышло!"

instance MonadIO m => HasClientManager (ReaderT Manager m) where
  getClientMaganer = ask

data ResAct a = ResActAdd a | ResActSet [a] | ResActTimeout | ResActNoUrls
  deriving (Show)

data ResSignal a = RSEnough [a] | RSTimeout [a] | RSNoUrls [a]

instance Foldable ResSignal where
  foldr f b sig = case sig of
    RSEnough  rs -> foldr f b rs
    RSTimeout rs -> foldr f b rs
    RSNoUrls  rs -> foldr f b rs

mergeResStoreMaybe :: Eq a => Int -> ResAct a -> Either [a] (ResSignal a) -> Maybe (Either [a] (ResSignal a))
mergeResStoreMaybe n act estore = case act of
  ResActAdd a -> case estore of
    Left xs' -> Just $ let xs = a:xs' in if length xs >= n then Right (RSEnough xs) else Left xs
    _ -> Nothing
  ResActSet xs  -> Just $ Left xs
  ResActTimeout -> either (Just . Right . RSTimeout) (const Nothing) estore
  ResActNoUrls  -> either (Just . Right . RSNoUrls) (const Nothing) estore

validateRes :: Eq a => [a] -> Either ClientMessages a
validateRes rs = case L.nub rs of
  []    -> Left CMSEmpty
  x:[]  -> Right x
  _     -> Left CMSValidationError

-- Implements request logic:
-- Request from n nodes and check if results are equal.
-- TODO: Add more sophisticated validation
requesterImpl :: forall t m e a b . (MonadFrontBase t m, Eq a, Show a)
  => (BaseUrl -> b -> ReaderT Manager IO (Either e a))   -- Request function
  -> Event t b                                      -- Request event
  -> m (Event t a)                                  -- Result
requesterImpl endpoint reqE = mdo
  uss           <- readExternalRef =<< getUrlsRef           -- list of urls
  (minN, maxN)  <- readExternalRef =<< getRequiredUrlNumRef -- Required number of confirmations
  dt            <- readExternalRef =<< getRequestTimeoutRef -- Get a timeout
  let rereqE = fforMaybe outE $ either Just (const Nothing)
      finE   = fforMaybe outE $ either (const Nothing) Just
  drawE <- delay 0.1 $ leftmost [reqE, rereqE]

  timeoutMsgE <- delay dt $ (True, CMSTimeout) <$ drawE
  timeoutE <- delay 1 timeoutMsgE
  performEvent $ (liftIO $ putStrLn "TIMEOOOOOUT") <$ timeoutE

  outE :: Event t (Either b a) <- fmap (switch . current) $ widgetHold (pure never) $ ffor drawE $ \req -> do
    toggleLoadingWidget timeoutMsgE
    urls  <- getRandUrls maxN uss                     -- Initial list of urls to query
    mng   <- getClientMaganer
    eresE <- fmap mconcat $ flip traverse urls $ \u -> do
      (ev, fire) <- newTriggerEvent
      void $ liftIO $ forkIO $ fire =<< flip runReaderT mng (endpoint u req)
      pure ev
    let failE = fforMaybe eresE $ \case
          Left err -> Just err
          _ -> Nothing
        succE = fforMaybe eresE $ \case
          Right res -> Just res
          _ -> Nothing
    rec   -- If a request fails then get a new url and try again
      let totalFailE = leftmost [failE, extraFailE]
      -- Collect used urls
      usedUrlsD <- foldDyn S.insert (S.fromList urls) extraUrlE
      -- Check if there are still unused urls and pick from them. If all urls were used at least once - shoutout
      murlE <- performEvent $ ffor (current usedUrlsD `tag` totalFailE) $ \used -> do
        let diff = S.difference uss used
        liftIO $ putStrLn $ show $ S.null diff
        if S.null diff then pure Nothing else fmap Just $ getExtraUrl diff
      let extraUrlE   = fmapMaybe id murlE
          noMoreUrlE  = fmapMaybe (maybe (Just ResActNoUrls) (const Nothing)) murlE
      -- Get extra results, split, feed extraFailE back into totalFailE
      -- extraResE <- endpointWrapper endpoint extraUrlD rereqE
      extraResE <- performEventAsync $ ffor extraUrlE $ \url cb ->
          void $ liftIO $ forkIO $ cb =<< flip runReaderT mng (endpoint url req)
      let extraFailE = fforMaybe extraResE $ \case
            Left err -> Just err
            _ -> Nothing
          extraSuccE = fforMaybe extraResE $ \case
            Right res -> Just res
            _ -> Nothing

    let storeActE = leftmost [ResActAdd <$> succE, ResActAdd <$> extraSuccE, ResActTimeout <$ timeoutE, noMoreUrlE]
    storeD <- foldDynMaybe (mergeResStoreMaybe maxN) (Left []) storeActE
    resE <- handleDangerMsg $ fforMaybe (updated storeD) $ \case
      Left _ -> Nothing
      Right act -> case act of
        RSEnough  rs -> Just $ validateRes rs
        RSTimeout rs -> if length rs >= minN then Just (validateRes rs) else Nothing
        RSNoUrls  rs -> if length rs >= minN then Just (validateRes rs) else Nothing

    -- Handle messages for loading display
    toggleLoadingWidget $ ffor reqE           $ \_        -> (True , CMSLoading 0 minN maxN)
    toggleLoadingWidget $ ffor totalFailE     $ \err      -> (True , CMSError)
    toggleLoadingWidget $ ffor (updated storeD) $ \store  -> let l = either length length store in (True , CMSLoading l minN maxN)
    toggleLoadingWidget $ ffor resE           $ \_        -> (False, CMSDone)


    liftIO $ do
      putStrLn $ show (minN, maxN)
      putStrLn $ show $ baseUrlPort <$> urls
    -- performEvent $ ffor extraUrlE $ liftIO . putStrLn . showBaseUrl
    -- performEvent $ ffor extraFailE $ const $ liftIO (print "FAAAAAIL")
    performEvent $ ffor noMoreUrlE $ const $ liftIO (print "NO MORE URLS")
    performEvent $ ffor storeActE $ liftIO . putStrLn . show
    performEvent $ ffor (updated usedUrlsD) $ liftIO . putStrLn . (++) "UsedD: " . show . fmap baseUrlPort . S.elems
    performEvent $ ffor murlE $ \case
      Just _ -> liftIO $ putStrLn "Murl: Just"
      Nothing -> liftIO $ putStrLn "Murl: Nothing"
    performEvent $ ffor (updated storeD) $ \case
      Left rs ->  liftIO $ putStrLn $ "Left: " ++ (show $ length rs)
      Right act -> liftIO $ putStrLn $ case act of
        RSEnough  rs  -> "RSEnough: " ++ (show $ length rs)
        RSTimeout rs  -> "RSTimeout: " ++ (show $ length rs)
        RSNoUrls  _   -> "RSNoUrls"

    delay 0.1 $ Right <$> resE

  pure finE

  where
    getExtraUrl :: MonadIO m => S.Set BaseUrl -> Performable m BaseUrl
    getExtraUrl uss = do
      liftIO $ putStrLn $ "DIFF: " ++ show (baseUrlPort <$> S.elems uss)
      u <- liftIO $ fmap (S.elems uss !!) $ getRandomR (0, length uss - 1)
      liftIO $ putStrLn $ show $ baseUrlPort u
      pure u

    getRandUrls :: MonadIO m => Int -> S.Set BaseUrl -> m [BaseUrl]
    getRandUrls n usSet = liftIO $ do
      let uss = S.elems usSet
      let perml = product [1 .. (length uss)] - 1
      i <- getRandomR (0, perml)
      pure $ take n $ L.permutations uss !! i
