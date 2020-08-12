module Ergvein.Index.Server.TCPService.MessageHandler where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Conversion

import Ergvein.Index.Server.Environment
import Ergvein.Index.API.Types
import Ergvein.Index.Protocol.Types as IPT
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockMetaRec]
getBlockMetaSlice currency startHeight endHeight = do
  let start = metaRecKey (currency, startHeight)
      end   = BlockMetaRecKey currency $ startHeight + pred endHeight
  slice <- safeEntrySlice start end
  let metaSlice = snd <$> slice
  pure metaSlice

handleMsg :: Message -> ServerM (Maybe Message)
handleMsg (PingMsg msg) = pure $ Just $ PongMsg msg

handleMsg (VersionMsg msg) = undefined

handleMsg (FiltersRequestMsg FilterRequestMessage {..}) = do
  slice <- getBlockMetaSlice undefined filterRequestMsgStart filterRequestMsgAmount
  let filters = V.fromList $ convert <$> slice

  pure $ Just $ FiltersResponseMsg $ FilterResponseMessage
    { filterResponseCurrency = filterRequestMsgCurrency
    , filterResponseFilters = filters
    }

handleMsg (FeeRequestMsg curs) = do
  fees <- liftIO . readTVarIO =<< asks envFeeEstimates
  let selCurs = M.restrictKeys fees $ S.fromList curs
  let resps = flip M.mapWithKey selCurs $ \cur fb -> case cur of
        IPT.BTC -> FeeRespBTC False fb
        IPT.TBTC -> FeeRespBTC True fb
        _ -> let FeeBundle (_, h) (_, m) (_, l) = fb
          in FeeRespGeneric cur h m l
  pure $ Just $ FeeResponseMsg $ M.elems resps
