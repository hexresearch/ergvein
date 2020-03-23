module System.X509.Android(
    getSystemCertificateStore
  ) where

import Control.Monad.IO.Class 
import Data.X509.CertificateStore (CertificateStore)

getSystemCertificateStore :: MonadIO m => m CertificateStore 
getSystemCertificateStore = undefined