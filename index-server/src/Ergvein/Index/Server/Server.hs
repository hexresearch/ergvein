module Ergvein.Index.Server.Server where

import Servant.Server
import Servant.API.Generic
import Servant.Server.Generic
import Ergvein.Index.API
import Ergvein.Index.API.V1
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Server.V1

ergveinIndexServer :: IndexVersionedApi AsServerM
ergveinIndexServer = IndexVersionedApi 
    { indexVersionedApi'v1 = toServant indexServer
    }