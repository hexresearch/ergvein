{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Headers.Btc.Types(
    initBtcDbs
  , getBtcHeadersDB
  , getBtcBestHeaderDB
  ) where

import Database.LMDB.Simple
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import qualified Codec.Serialise as S
import qualified Data.Serialize as Cereal

btcHeadersDBName :: String
btcHeadersDBName = "btcheaders"

btcBestHeaderDBName :: String
btcBestHeaderDBName = "btcbestheader"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  fdb <- getBtcHeadersDB
  bdb <- getBtcBestHeaderDB
  bdb `seq` fdb `seq` pure ()

getBtcHeadersDB :: Mode mode => Transaction mode (Database BlockHash BlockNode)
getBtcHeadersDB = getDatabase $ Just btcHeadersDBName

getBtcBestHeaderDB :: Mode mode => Transaction mode (Database () BlockNode)
getBtcBestHeaderDB = getDatabase $ Just btcBestHeaderDBName

deriving instance S.Serialise BlockHash

instance S.Serialise Hash256 where
  encode = S.encode . Cereal.encode
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ Cereal.decode bs
  {-# INLINE decode #-}

instance S.Serialise BlockNode where
  encode = S.encode . Cereal.encode
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ Cereal.decode bs
  {-# INLINE decode #-}
