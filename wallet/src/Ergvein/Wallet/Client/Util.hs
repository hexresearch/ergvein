{-# LANGUAGE UndecidableInstances #-}

module Ergvein.Wallet.Client.Util
  (
  -- * Clients
    indexerVClient
  , indexerVClient_
  , indexerV1
  , indexerV1_
  -- * Helpers
  , dangerResult
  , textifyResult
  , endpoint0
  , endpoint0'
  , endpoint1
  , endpoint1'
  , endpoint2
  , endpoint2'
  , endpoint3
  , endpoint3'
  , endpoint4
  , endpoint4'
  -- * Error type
  , ClientError(..)
  -- * Reexports
  , module Reexport
  ) where

import Control.Monad                as Reexport
import Control.Monad.IO.Class       as Reexport
import Data.Monoid
import Data.Proxy                   as Reexport
import Data.Text (Text)
import GHC.TypeLits                 as Reexport
import Reflex                       as Reexport hiding (Response)
import Reflex.Dom                   as Reexport hiding (askEvents, Response, Client)
import Reflex.Localize
import Servant.API
import Servant.API.Generic          as Reexport
import Servant.Reflex               as Reexport

import Ergvein.Index.API
import Ergvein.Index.API.V1
import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Monad.Base

-- | Shortcut for request body
type DBody t a = Dynamic t (Either Text a)
-- | Shortcut for request capture clause
type DCapture t a = DBody t a
-- | Shortcut for query body
type DParam t a = Dynamic t (QParam a)
-- | Shortcut for endpoint ending
type DEndpoint t m a =  Event t () -> m (Event t (ReqResult () a))

-- | Helper for servant-generic to convert API structs into servant-reflex client
data AsReflex (t :: *) (m :: * -> *) (tag :: *)

-- | Swagger summary is ignored for client
instance HasClient t m sublayout tag
  => HasClient t m (Summary desc :> sublayout) tag where

  type Client t m (Summary desc :> sublayout) tag =
    Client t m sublayout tag

  clientWithRouteAndResultHandler Proxy =
    clientWithRouteAndResultHandler (Proxy :: Proxy sublayout)

instance GenericMode (AsReflex t m tag) where
  type AsReflex t m tag :- api = Client t m api tag

data ClientError = ClientError Text
  deriving (Eq)

instance LocalizedPrint ClientError where
  localizedShow _ (ClientError t) = t

type ClientHandler t m = AlertHandler t m ClientError

-- | Derive client endpoints for given monad
indexerVClient :: forall t m tag . MonadBaseConstr t m => Proxy m -> Proxy tag -> Dynamic t Text -> IndexVersionedApi (AsReflex t m tag)
indexerVClient pm ptag durl = fromServant $ client (Proxy :: Proxy (ToServantApi IndexVersionedApi)) pm ptag $ BasePath <$> durl

-- | Derived client with no tag
indexerVClient_ :: forall t m . MonadBaseConstr t m => Proxy m -> Dynamic t Text -> IndexVersionedApi (AsReflex t m ())
indexerVClient_ pm = indexerVClient pm (Proxy :: Proxy ())

indexerV1 :: forall t m tag . MonadBaseConstr t m => Proxy m -> Proxy tag -> Dynamic t Text -> IndexApi (AsReflex t m tag)
indexerV1 pm ptag durl = fromServant . indexVersionedApi'v1 $ indexerVClient pm ptag durl

-- | Derived client with no tag
indexerV1_ :: forall t m . MonadBaseConstr t m => Proxy m -> Dynamic t Text -> IndexApi (AsReflex t m ())
indexerV1_ pm = fromServant . indexVersionedApi'v1 . indexerVClient pm (Proxy :: Proxy ())

-- | Convert req result to text
textifyResult :: ReqResult () a -> Either ClientError a
textifyResult r = case r of
  ResponseSuccess _ a _ -> Right a
  ResponseFailure _ e xhr -> case _xhrResponse_responseText xhr of
    Just body -> Left $ either ClientError ClientError $ text2json body
    Nothing -> Left $ ClientError e
  RequestFailure _ e -> Left $ ClientError e

-- | Display error in danger panel
dangerResult :: ClientHandler t m => Event t (ReqResult () a) -> m (Event t a)
dangerResult = handleDangerMsg . fmap textifyResult

instance Functor QParam where
  fmap f v = case v of
    QParamSome a -> QParamSome (f a)
    QNone -> QNone
    QParamInvalid e -> QParamInvalid e

-- | Helper for wrapping endpoint without authorisation
endpoint0 :: forall t m a. ClientHandler t m
  => DEndpoint t m a -- ^ Endpoint
  -> Event t () -- ^ Fire event
  -> m (Event t a) -- ^ Result
endpoint0 endpoint bodyE = handleDangerMsg =<< endpoint0' endpoint bodyE

-- | Helper for wrapping endpoint without authorisation
endpoint1 :: forall t m a b. ClientHandler t m
  => (DBody t a -> DEndpoint t m b) -- ^ Endpoint
  -> Event t a -- ^ Argument
  -> m (Event t b) -- ^ Result
endpoint1 endpoint bodyE = handleDangerMsg =<< endpoint1' endpoint bodyE

-- | Helper for wrapping endpoint without authorisation
endpoint2 :: forall t m a b c . ClientHandler t m
  => (DBody t a -> DBody t b -> DEndpoint t m c) -- ^ Endpoint
  -> Event t (a, b) -- ^ Argument
  -> m (Event t c) -- ^ Result
endpoint2 endpoint bodyE = handleDangerMsg =<< endpoint2' endpoint bodyE

-- | Helper for wrapping endpoint without authorisation
endpoint3 :: forall t m a b c d . ClientHandler t m
  => (DBody t a -> DBody t b -> DBody t c -> DEndpoint t m d) -- ^ Endpoint
  -> Event t (a, b, c) -- ^ Argument
  -> m (Event t d) -- ^ Result
endpoint3 endpoint bodyE = handleDangerMsg =<< endpoint3' endpoint bodyE

-- | Helper for wrapping endpoint without authorisation
endpoint4 :: forall t m a b c d e . ClientHandler t m
  => (DBody t a -> DBody t b -> DBody t c -> DBody t d -> DEndpoint t m e) -- ^ Endpoint
  -> Event t (a, b, c, d) -- ^ Argument
  -> m (Event t e) -- ^ Result
endpoint4 endpoint bodyE = handleDangerMsg =<< endpoint4' endpoint bodyE

-- | Helper for wrapping endpoint without authorisation
endpoint0' :: forall t m a. MonadBaseConstr t m
  => DEndpoint t m a -- ^ Endpoint
  -> Event t () -- ^ Fire event
  -> m (Event t (Either ClientError a)) -- ^ Result
endpoint0' endpoint fireE = fmap textifyResult <$> endpoint fireE

-- | Helper for wrapping endpoint without authorisation
endpoint1' :: forall t m a b. MonadBaseConstr t m
  => (DBody t a -> DEndpoint t m b) -- ^ Endpoint
  -> Event t a -- ^ Argument
  -> m (Event t (Either ClientError b)) -- ^ Result
endpoint1' endpoint bodyE = do
  bodyD <- holdDyn (Left "") $ Right <$> bodyE
  resE <- endpoint bodyD $ void bodyE
  pure $ textifyResult <$> resE

-- | Helper for wrapping endpoint without authorisation
endpoint2' :: forall t m a b c . MonadBaseConstr t m
  => (DBody t a -> DBody t b -> DEndpoint t m c) -- ^ Endpoint
  -> Event t (a, b) -- ^ Argument
  -> m (Event t (Either ClientError c)) -- ^ Result
endpoint2' endpoint bodyE = do
  arg1D <- holdDyn (Left "") $ Right . fst <$> bodyE
  arg2D <- holdDyn (Left "") $ Right . snd <$> bodyE
  resE <- endpoint arg1D arg2D $ void bodyE
  pure $ textifyResult <$> resE

-- | Helper for wrapping endpoint without authorisation
endpoint3' :: forall t m a b c d . MonadBaseConstr t m
  => (DBody t a -> DBody t b -> DBody t c -> DEndpoint t m d) -- ^ Endpoint
  -> Event t (a, b, c) -- ^ Argument
  -> m (Event t (Either ClientError d)) -- ^ Result
endpoint3' endpoint bodyE = do
  arg1D <- holdDyn (Left "") $ Right . (\(v,_,_) -> v) <$> bodyE
  arg2D <- holdDyn (Left "") $ Right . (\(_,v,_) -> v) <$> bodyE
  arg3D <- holdDyn (Left "") $ Right . (\(_,_,v) -> v) <$> bodyE
  resE <- endpoint arg1D arg2D arg3D $ void bodyE
  pure $ textifyResult <$> resE

-- | Helper for wrapping endpoint without authorisation
endpoint4' :: forall t m a b c d e . MonadBaseConstr t m
  => (DBody t a -> DBody t b -> DBody t c -> DBody t d -> DEndpoint t m e) -- ^ Endpoint
  -> Event t (a, b, c, d) -- ^ Argument
  -> m (Event t (Either ClientError e)) -- ^ Result
endpoint4' endpoint bodyE = do
  arg1D <- holdDyn (Left "") $ Right . (\(v,_,_,_) -> v) <$> bodyE
  arg2D <- holdDyn (Left "") $ Right . (\(_,v,_,_) -> v) <$> bodyE
  arg3D <- holdDyn (Left "") $ Right . (\(_,_,v,_) -> v) <$> bodyE
  arg4D <- holdDyn (Left "") $ Right . (\(_,_,_,v) -> v) <$> bodyE
  resE <- endpoint arg1D arg2D arg3D arg4D $ void bodyE
  pure $ textifyResult <$> resE
