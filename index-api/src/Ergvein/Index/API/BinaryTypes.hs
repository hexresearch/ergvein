module Ergvein.Index.API.BinaryTypes where

import Data.Word
import Data.ByteString
import Data.Vector.Unboxed
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


codeBTC :: ByteString
codeBTC    = "BTC     " --Bitcoin
codeTBTC :: ByteString
codeTBTC   = "TBTC    " --Testnet Bitcoin
codeERGO :: ByteString
codeERGO   = "ERGO    " --Ergo
codeTERGO :: ByteString
codeTERGO  = "TERGO   " --Testnet Ergo
codeUSDTO :: ByteString
codeUSDTO  = "USDTO   " --Omni USDT
codeTUSDTO :: ByteString
codeTUSDTO = "TUSDTO  " --Testnet Omni USDT
codeLT :: ByteString
codeLT     = "LT      " --Litecoin
codeTLT :: ByteString
codeTLT    = "TLT     " --Testnet Litecoin
codeZEC :: ByteString
codeZEC    = "ZEC     " --ZCash
codeTZEC :: ByteString
codeTZEC   = "TZEC    " --Testnet ZCash
codeCPR :: ByteString
codeCPR    = "CPR     " --Cypra token
codeTCPR :: ByteString
codeTCPR   = "TCPR    " --Testnet Cypra

verACKMsg :: Builder
verACKMsg = word32BE msgType 
         <> word32BE 0
  where
    msgType = 0

pingMsg :: Word64 -> Builder
pingMsg nonce = word32BE msgType 
             <> word32BE 4 
             <> word64BE nonce
  where
    msgType = 11

pongMsg :: Word64 -> Builder
pongMsg nonce = word32BE msgType 
             <> word32BE 4 
             <> word64BE nonce
  where
    msgType = 12