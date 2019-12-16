module Ergvein.Interfaces.Ergo.It.Api.NodeApi where

import Data.Aeson as A
import Data.Int

-- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/it/scala/org/ergoplatform/it/api/NodeApi.scala#L175
data NodeInfo = NodeInfo {
  bestHeaderId     :: Maybe String
, bestBlockId      :: Maybe String
, bestHeaderHeight :: Maybe Int32
, bestBlockHeight  :: Maybe Int32
, stateRoot        :: Maybe String
}

instance FromJSON NodeInfo where
  parseJSON = withObject "NodeInfo" $ \o -> do
    -- bestHeaderIdOpt <- c.downField("bestHeaderId").as[Option[String]]
    bestHeaderId <- o .:? "bestHeaderId"
    -- bestBlockIdOpt <- c.downField("bestFullHeaderId").as[Option[String]]
    bestBlockId <- o .:? "bestFullHeaderId"
    -- bestHeaderHeightOpt <- c.downField("headersHeight").as[Option[Int]]
    bestHeaderHeight <- o .:? "headersHeight"
    -- bestBlockHeightOpt <- c.downField("fullHeight").as[Option[Int]]
    bestBlockHeight <- o .:? "fullHeight"
    -- stateRootOpt <- c.downField("stateRoot").as[Option[String]]
    stateRoot <- o .:? "stateRoot"
    -- } yield NodeInfo(bestHeaderIdOpt, bestBlockIdOpt, bestHeaderHeightOpt, bestBlockHeightOpt, stateRootOpt)
    pure NodeInfo {..}
  {-# INLINE parseJSON #-}
