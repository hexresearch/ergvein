module Ergvein.Index.API.Types where

import Ergvein.Aeson
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import GHC.Generics
import Data.Word

-- Height
newtype HeightRequest = HeightRequest
    { heightReqCurrency :: Currency
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "heightReq") ''HeightRequest)

newtype HeightResponse = HeightResponse
    { heightRespHeight :: BlockHeight
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "heightResp") ''HeightResponse)

-- Filters
data BlockFiltersRequest = BlockFiltersRequest
    { filtersReqCurrency         :: !Currency
    , filtersReqStartHeight      :: !BlockHeight
    , filtersReqAmount           :: !Word64
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "filtersReq") ''BlockFiltersRequest)

type BlockFiltersResponse = [(BlockHash, AddressFilterHexView)]

--Info
data ScanProgressItem = ScanProgressItem
    { scanProgressCurrency      :: !Currency
    , scanProgressScannedHeight :: !BlockHeight
    , scanProgressActualHeight  :: !BlockHeight
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "infoRespScanProgressItem") ''ScanProgressItem)

data InfoResponse = InfoResponse
    { infoScanProgress  :: [ScanProgressItem]
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "infoResp") ''InfoResponse)
