{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Monad.Class(
    MonadFrontBase
  , MonadFront
  ) where

import Control.Monad.IO.Class
import Reflex
import Reflex.Spider.Internal (SpiderHostFrame, Global)
import Foreign.JavaScript.TH (WithJSContextSingleton)

import Ergvein.Core
import Ergvein.Wallet.Version
import Ergvein.Wallet.Monad.Env
import Sepulcas.Monad


import qualified Crypto.Random.Types as CR
import qualified Reflex.Profiled as RP

-- Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadFrontBase t m = (
    PlatformNatives
  , HasVersion
  , MonadSettings t m
  , MonadClient t m
  , Sepulcable t m
  , CR.MonadRandom (Performable m)
  , HasStoreDir (Performable m)
  , HasSepulca t m
  , MonadPreWallet t m
  , HasPreWalletEnv t m
  , HasClientEnv t m
  , HasPassEnv t m
  , HasBaseEnv t m
  )

-- | Authorized context. Has access to storage and indexer's functionality
type MonadFront t m = (
    MonadFrontBase t m
  , MonadWallet t m
  , MonadStatus t m
  , MonadNode t m
  , MonadStorage t m
  , MonadClient t m
  , HasNodeEnv t m
  , HasWalletEnv t m
  , HasStoreEnv t m
  , HasStatusEnv t m
  )

-- ===========================================================================
--    Helper instances for base monad
-- ===========================================================================

instance CR.MonadRandom (WithJSContextSingleton x (SpiderHostFrame Global)) where
  getRandomBytes = liftIO . CR.getRandomBytes

instance CR.MonadRandom (WithJSContextSingleton x (RP.ProfiledM (SpiderHostFrame Global))) where
  getRandomBytes = liftIO . CR.getRandomBytes
