module System.X509.Android(
    getSystemCertificateStore
  , decodePemCerts
  , SignedCertificate
  ) where

import Android.HaskellActivity
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.PEM (pemParseBS, pemContent)
import Data.X509 (SignedCertificate, decodeSignedCertificate)
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Foreign
import Foreign.C

import qualified Data.ByteString as BS

newtype JString = JString { unJString :: Ptr JString }

foreign import ccall safe "android_load_certs" androidLoadCerts :: HaskellActivity -> IO ()
foreign import ccall safe "android_certs_length" androidCertsLength :: IO Int
foreign import ccall safe "android_get_cert" androidGetCert :: Int -> IO JString
foreign import ccall safe "read_jstring" androidReadJString :: JString -> IO CString
foreign import ccall safe "release_jstring" androidReleaseJString :: JString -> CString -> IO ()

readCertString :: Int -> IO ByteString
readCertString i = do
  cjstr <- androidGetCert i
  cstr <- androidReadJString cjstr
  str <- BS.packCString cstr
  str `seq` androidReleaseJString cjstr cstr
  pure str

getSystemCertificateStore :: MonadIO m => [SignedCertificate] -> m CertificateStore
getSystemCertificateStore extcerts = liftIO $ do
  androidLoadCerts =<< getHaskellActivity
  i <- androidCertsLength
  cs <- traverse loadCert [0 .. i-1]
  let totalCerts = extcerts <> concat cs
  pure $ makeCertificateStore totalCerts
  where
    loadCert i = do
      str <- readCertString i
      pure $ either (const []) id $ decodePemCerts str

decodePemCerts :: ByteString -> Either String [SignedCertificate]
decodePemCerts bs = do
  pems <- pemParseBS bs
  traverse (decodeSignedCertificate . pemContent) pems
