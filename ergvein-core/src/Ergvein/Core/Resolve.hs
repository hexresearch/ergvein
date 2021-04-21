module Ergvein.Core.Resolve(
    mkResolvSeed
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Network.DNS
import Reflex.ExternalRef
import Reflex.Main.Thread
import Sepulcas.Native

import qualified Data.Set as S

mkResolvSeed :: (MonadHasMain m, MonadHasSettings t m) => m ResolvSeed
mkResolvSeed = do
  defDns <- fmap (S.toList . settingsDns) $ readExternalRef =<< getSettingsRef
  if isAndroid
    then do
      dns <- liftIO . wait =<< runOnMainThreadA androidDetectDns
      liftIO $ makeResolvSeed defaultResolvConf {
          resolvInfo = RCHostNames $ case dns of
            [] -> defDns
            _ -> dns
        , resolvConcurrent = True
        }
  else liftIO $ do
    rs <- makeResolvSeed $ defaultResolvConf { resolvConcurrent = True}
    res :: Either SomeException () <- try $ withResolver rs $ const $ pure ()
    case res of
      Right _ -> pure rs
      Left _ ->  makeResolvSeed $ defaultResolvConf { resolvInfo = RCHostNames defDns, resolvConcurrent = True}
{-# INLINE mkResolvSeed #-}
