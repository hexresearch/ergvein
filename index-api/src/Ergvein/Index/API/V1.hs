module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.Types

type Body a = ReqBody '[JSON] a
type PostResp a = Post '[JSON] a


type IndexGetHeight         = "height"    :> Body HeightRequest :> PostResp HeightResponse

type IndexGetBlockFilters   = "filters"   :> Body BlockFiltersRequest :> PostResp BlockFiltersResponse

type IndexGetInfo           = "info"      :> PostResp InfoResponse

type IndexAddPeer           = "addPeer"   :> PostResp ()

data IndexApi route = IndexApi
    { indexGetHeight         :: route :- IndexGetHeight
    , indexGetBlockFilters   :: route :- IndexGetBlockFilters
    , indexGetInfo           :: route :- IndexGetInfo
    , indexAddPeer           :: route :- IndexAddPeer
    } deriving Generic
