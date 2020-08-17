module Ergvein.Index.Server.TCPService.MessageHandler where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Conversion
import Data.Time.Clock.POSIX
import Control.Monad.Random

import Ergvein.Index.API.Types
import Ergvein.Index.Protocol.Types as IPT
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.BlockchainScanning.Types

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockMetaRec]
getBlockMetaSlice currency startHeight endHeight = do
  let start = metaRecKey (currency, startHeight)
      end   = BlockMetaRecKey currency $ startHeight + pred endHeight
  slice <- safeEntrySlice start end
  let metaSlice = snd <$> slice
  pure metaSlice

handleMsg :: Message -> ServerM (Maybe Message)
handleMsg (MPing msg) = pure $ Just $ MPong msg

handleMsg (MPong _) = pure Nothing

handleMsg (MVersionACK _) = pure Nothing

handleMsg (MVersion msg) = pure Nothing

handleMsg (MFiltersRequest FilterRequest {..}) = do
  let currency = convert filterRequestMsgCurrency
  slice <- getBlockMetaSlice currency filterRequestMsgStart filterRequestMsgAmount
  let filters = V.fromList $ convert <$> slice

  pure $ Just $ MFiltersResponse $ FilterResponse
    { filterResponseCurrency = filterRequestMsgCurrency
    , filterResponseFilters = filters
    }

handleMsg (MFeeRequest curs) = do
  fees <- liftIO . readTVarIO =<< asks envFeeEstimates
  let selCurs = M.restrictKeys fees $ S.fromList curs
  let resps =  (`M.mapWithKey` selCurs) $ \cur fb -> case cur of
        IPT.BTC -> FeeRespBTC False fb
        IPT.TBTC -> FeeRespBTC True fb
        _ -> let FeeBundle (_, h) (_, m) (_, l) = fb
          in FeeRespGeneric cur h m l
  pure $ Just $ MFeeResponse $ M.elems resps

ownVersion :: ServerM Version
ownVersion = do
  now   <- liftIO $ round <$> getPOSIXTime
  nonce <- liftIO $ randomIO
  time  <- liftIO $ fromIntegral . floor <$> getPOSIXTime
  
  scanNfo <- UV.fromList . fmap convert <$> scanningInfo

  pure $ Version {
      versionVersion    = protocolVersion
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = scanNfo
    }