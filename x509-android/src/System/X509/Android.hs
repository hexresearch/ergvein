module System.X509.Android(
    getSystemCertificateStore
  ) where

import Android.HaskellActivity
import Control.Monad.IO.Class 
import Data.X509.CertificateStore (CertificateStore)

foreign import ccall safe "android_load_certs" androidLoadCerts :: HaskellActivity -> IO ()
foreign import ccall safe "android_certs_length" androidCertsLength :: IO Int
foreign import ccall safe "android_get_cert" androidGetCert :: Int -> IO JString
foreign import ccall safe "read_jstring" androidReadJString :: JString -> IO CString
foreign import ccall safe "release_jstring" androidReleaseJString :: JString -> CString -> IO ()

getSystemCertificateStore :: MonadIO m => m CertificateStore 
getSystemCertificateStore = liftIO $ do 
  androidLoadCerts =<< getHaskellActivity
  i <- androidCertsLength
  cs <- traverse loadCert [0 .. i-1]
  where 
    loadCert i = pure ()