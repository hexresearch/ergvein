module Ergvein.Index.Server.TCPService.MessageHandler where

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Monad

handleMsg :: Message -> ServerM (Maybe Message)
handleMsg (PingMsg msg) = pure $ Just $ PongMsg msg

handleMsg (VersionMsg msg) = undefined

handleMsg (FiltersRequestMsg msg) = undefined

handleMsg (FiltersResponseMsg msg) = undefined