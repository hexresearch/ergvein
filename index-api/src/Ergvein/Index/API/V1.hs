module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.Types

type Body a = ReqBody '[JSON] a
type PostResp a = Post '[JSON] a

type IndexGetHeight         = "height"    :> Body HeightRequest :> PostResp HeightResponse

type IndexGetBlockFilters   = "filters"   :> Body BlockFiltersRequest :> PostResp BlockFiltersResponse

type IndexGetInfo           = "info"      :> PostResp InfoResponse

type IndexIntroducePeer     = "introducePeer"   :> Body IntroducePeerReq :> PostResp IntroducePeerResp

type IndexKnownPeers        = "knownPeers" :> Body KnownPeersReq :> PostResp KnownPeersResp

type IndexGetFees           = "fees"      :> PostResp IndexFeesResp

data IndexApi route = IndexApi
    { indexGetHeight         :: route :- IndexGetHeight
    , indexGetBlockFilters   :: route :- IndexGetBlockFilters
    , indexGetInfo           :: route :- IndexGetInfo
    , indexIntroducePeer     :: route :- IndexIntroducePeer
    , indexKnownPeers        :: route :- IndexKnownPeers
    , indexGetFees           :: route :- IndexGetFees
    } deriving Generic
