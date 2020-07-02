module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.Types
import Ergvein.Types.Currency

type Body a = ReqBody '[JSON] a
type PostResp a = Post '[JSON] a
type GetResp a = Get '[JSON] a

type IndexGetHeight         = "height"    :> Body HeightRequest :> PostResp HeightResponse

type IndexGetBlockFilters   = "filters"   :> Body BlockFiltersRequest :> PostResp BlockFiltersResponse

type IndexPing              = "ping"      :> GetResp ()

type IndexGetInfo           = "info"      :> GetResp InfoResponse

type IndexIntroducePeer     = "introducePeer"   :> Body IntroducePeerReq :> PostResp IntroducePeerResp

type IndexKnownPeers        = "knownPeers" :> Body KnownPeersReq :> PostResp KnownPeersResp

type IndexGetFees           = "fees"      :> Body [Currency] :> PostResp IndexFeesResp

data IndexApi route = IndexApi
    { indexGetHeight         :: route :- IndexGetHeight
    , indexGetBlockFilters   :: route :- IndexGetBlockFilters
    , indexGetInfo           :: route :- IndexGetInfo
    , indexPing              :: route :- IndexPing
    , indexIntroducePeer     :: route :- IndexIntroducePeer
    , indexKnownPeers        :: route :- IndexKnownPeers
    , indexGetFees           :: route :- IndexGetFees
    } deriving Generic
