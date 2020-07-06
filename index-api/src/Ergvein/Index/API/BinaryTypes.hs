module Ergvein.Index.API.BinaryTypes where

import Data.Word
import Data.ByteString
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.ByteString.Builder
import Foreign.Storable

import qualified Data.ByteString.Lazy               as L

data MessageType = Version
                 | VersionACK
                 | FiltersRequest
                 | FiltersResponse
                 | FilterEvent
                 | PeerRequest
                 | PeerResponse
                 | FeeRequest
                 | FeeResponse
                 | IntroducePeer
                 | Reject
                 | Ping
                 | Pong


codeBTC    :: ByteString
codeBTC    = "BTC     " --Bitcoin
codeTBTC   :: ByteString
codeTBTC   = "TBTC    " --Testnet Bitcoin
codeERGO   :: ByteString
codeERGO   = "ERGO    " --Ergo
codeTERGO  :: ByteString
codeTERGO  = "TERGO   " --Testnet Ergo
codeUSDTO  :: ByteString
codeUSDTO  = "USDTO   " --Omni USDT
codeTUSDTO :: ByteString
codeTUSDTO = "TUSDTO  " --Testnet Omni USDT
codeLT     :: ByteString
codeLT     = "LT      " --Litecoin
codeTLT    :: ByteString
codeTLT    = "TLT     " --Testnet Litecoin
codeZEC    :: ByteString
codeZEC    = "ZEC     " --ZCash
codeTZEC   :: ByteString
codeTZEC   = "TZEC    " --Testnet ZCash
codeCPR    :: ByteString
codeCPR    = "CPR     " --Cypra token
codeTCPR   :: ByteString
codeTCPR   = "TCPR    " --Testnet Cypra

word32sizeOf :: (Storable a) => a -> Word32
word32sizeOf = fromIntegral . sizeOf

scanBlock :: Word32 -> Word32 -> Word64 -> Word64 -> Builder
scanBlock currency version scanHeight height = word32BE currency
                                            <> word32BE version
                                            <> word64BE scanHeight
                                            <> word64BE height

verMsg :: Word32 ->  Word64 -> Word64 -> V.Vector Builder -> Builder
verMsg version time nonce scanBlocks = 
  msg msgType msgSize $ word32BE version
                     <> word64BE time
                     <> word64BE nonce
                     <> word32BE currenciesAmount <> V.foldl (<>) mempty scanBlocks
  where
    msgType = 0
    scanBlockSize = 24
    currenciesAmount = fromIntegral $ V.length scanBlocks
    msgSize = word32sizeOf time + word32sizeOf nonce + currenciesAmount * scanBlockSize

verACKMsg :: Builder
verACKMsg = msg msgType msgSize mempty
  where
    msgType = 1
    msgSize = 0

pingMsg :: Word64 -> Builder
pingMsg nonce = msg msgType msgSize $ word64BE nonce
  where
    msgType = word32sizeOf nonce
    msgSize = 4

pongMsg :: Word64 -> Builder
pongMsg nonce = msg msgType msgSize $ word64BE nonce
  where
    msgType = 12
    msgSize = word32sizeOf nonce

msg :: Word32 -> Word32 -> Builder -> Builder
msg msgType msgLength payload = word32BE msgType <> word32BE msgLength <> payload 