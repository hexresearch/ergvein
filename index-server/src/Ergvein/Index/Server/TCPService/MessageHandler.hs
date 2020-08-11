module Ergvein.Index.Server.TCPService.MessageHandler where

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.API.Types
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction
import qualified Data.Vector as V
import Conversion
import Ergvein.Index.Server.TCPService.Conversions

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