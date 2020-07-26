module Ergvein.Index.Server.TCPService.MessageHandler where

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Monad

handleMsg :: Message -> ServerM Message
handleMsg (PingMsg msg) = pure $ PongMsg msg

handleMsg (VersionMsg msg) = undefined

handleMsg (FiltersRequestMsg msg) = undefined

handleMsg (FiltersResponseMsg msg) = undefined