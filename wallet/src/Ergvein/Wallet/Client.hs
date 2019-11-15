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
  = CMSLoading Int Int
  | CMSError
  | CMSEmpty
  | CMSValidationError
  | CMSDone
  deriving (Eq)

instance LocalizedPrint ClientMessages where
  localizedShow l v = case l of
    English -> case v of
      CMSLoading i n      -> "Loading: " <> showt i <> " of " <> showt n
      CMSError            -> "A request has failed"
      CMSEmpty            -> "Results are empty"
      CMSValidationError  -> "Validation error: inconsistent results"
      CMSDone             -> "Done!"
    Russian -> case v of
      CMSLoading i n      -> "Запрашиваю. " <> showt i <> " из " <> showt n <> " ответили."
      CMSError            -> "Один из запросов не удался"
      CMSEmpty            -> "Результатов нет"
      CMSValidationError  -> "Ошибка: противоречивые ответы"
      CMSDone             -> "Готово!"

instance MonadIO m => HasClientManager (ReaderT Manager m) where
  getClientMaganer = ask

endpointWrapper :: (MonadFrontBase t m)
  => (BaseUrl -> b -> ReaderT Manager IO (Either e a))
  -> Dynamic t (Maybe BaseUrl)
  -> Event t b
  -> m (Event t (Either e a))
endpointWrapper endpoint urlD reqE = do
  let urlReqE = attachWithMaybe (\mu r -> (,r) <$> mu) (current urlD) reqE
  performEventAsync $ ffor urlReqE $ \(url,req) cb -> do
    mng <- getClientMaganer
    void $ liftIO $ forkIO $ cb =<< flip runReaderT mng (endpoint url req)

-- Implements request logic:
-- Request from n nodes and check if results are equal.
-- TODO: Add more sophisticated validation
requesterImpl :: (MonadFrontBase t m, Eq a)
  => (BaseUrl -> b -> ReaderT Manager IO (Either e a))   -- Request function
  -> Event t b                                      -- Request event
  -> m (Event t a)                                  -- Result
requesterImpl endpoint reqE = do
  reqD  <- holdDyn Nothing $ Just <$> reqE          -- Hold request value for later
  uss   <- readExternalRef =<< getUrlsRef           -- list of urls
  n     <- readExternalRef =<< getRequiredUrlNumRef -- Required number of confirmations
  urls  <- getRandUrls n uss                        -- Initial list of urls to query

  -- Get a response event eresE :: Event t (Either e a) and split into failure and success events
  eresE <- fmap mconcat $ flip traverse urls $ \u -> endpointWrapper endpoint (pure $ Just u) reqE

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
    -- Check if there are still unused urls and pick from them. If all urls were used at least once - pick from all urls
    extraUrlE <- performEvent $ ffor (current usedUrlsD `tag` totalFailE) $ \used -> do
      let diff = S.difference uss used
      getExtraUrl $ if S.null diff then uss else diff

    extraUrlD <- holdDyn Nothing $ fmap Just extraUrlE
    -- Wait a bit after totalFailE before firing another request, so that extraUrlD holds new url
    rereqE    <- delay 0.01 $ fmapMaybe id $ current reqD `tag` totalFailE
    -- Get extra results, split, feed extraFailE back into totalFailE
    extraResE <- endpointWrapper endpoint extraUrlD rereqE
    let extraFailE = fforMaybe extraResE $ \case
          Left err -> Just err
          _ -> Nothing
        extraSuccE = fforMaybe extraResE $ \case
          Right res -> Just res
          _ -> Nothing

  -- Collect successful results
  resD <- foldDyn (:) [] $ leftmost [succE, extraSuccE]
  -- When there is enough result, run validation and fire the final event away
  resE <- handleDangerMsg $ fforMaybe (updated resD) $ \rs -> if length rs >= n then Just (validateRes rs) else Nothing

  -- Handle messages for loading display
  toggleLoadingWidget $ ffor reqE           $ \_        -> (True , CMSLoading 0 n)
  toggleLoadingWidget $ ffor totalFailE     $ \err      -> (True , CMSError)
  toggleLoadingWidget $ ffor (updated resD) $ \rs       -> (True , CMSLoading (length rs) n)
  toggleLoadingWidget $ ffor resE           $ \_        -> (False, CMSDone)

  delay 0.1 resE

  where
    validateRes :: Eq a => [a] -> Either ClientMessages a
    validateRes rs = case L.nub rs of
      []    -> Left CMSEmpty
      x:[]  -> Right x
      _     -> Left CMSValidationError

    getExtraUrl :: MonadIO m => S.Set BaseUrl -> m BaseUrl
    getExtraUrl uss = liftIO $ fmap (S.elems uss !!) $ getRandomR (0, length uss - 1)

    getRandUrls :: MonadIO m => Int -> S.Set BaseUrl -> m [BaseUrl]
    getRandUrls n usSet = liftIO $ do
      let uss = S.elems usSet
      let perml = product [1 .. (length uss)] - 1
      i <- getRandomR (0, perml)
      pure $ take n $ L.permutations uss !! i
