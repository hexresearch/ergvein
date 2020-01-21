{-# LANGUAGE TypeApplications #-}

module Tests.Ergo.Api where

import Test.Tasty.Hspec

import Control.Monad
import Data.Aeson as A
import Data.Either.Combinators
import Data.Text (Text)

import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (q)

import Ergvein.Interfaces.Ergo.Api
import Ergvein.Interfaces.Ergo.Scorex.Util.Package


spec_BlockHeaderParser :: Spec
spec_BlockHeaderParser = do
    it "Decode BlockHeader from json sample" $
        (leftToMaybe . A.eitherDecode @BlockHeader $ sample) `shouldBe` Nothing
  where
    sample = [q|
        {
          "extensionId": "f60ce4b63c29ff7f47b7dab90418065f22986ed6379b592bddd8fe33fe357ac4",
          "difficulty": "1199990374400",
          "votes": "000000",
          "timestamp": 1561979029859,
          "size": 279,
          "stateRoot": "346be81869e254cbb192ee5013a9b48f07fb66e0f809b00a63d92649e12129b704",
          "height": 6,
          "nBits": 100734821,
          "version": 1,
          "id": "918d0c4ccb9a26cc69a3250eef1117b07bf843367a25455fd0873349a0821a61",
          "adProofsRoot": "8f118c8c15e83b19c467f35ac81ff3d88ea344b4ae88231cb08304cdb1aca961",
          "transactionsRoot": "296a3e3d6485b9c32a67d18f09af8758ba5f393bbcb29ed4ddcd6e357877e70f",
          "extensionHash": "df4ff3b77824042f5c16a5da006c992258bd8574e8429b59cd02fc59ff0d22ce",
          "powSolutions": {
            "pk": "022846c4f17a909080d7cb8bcf6217e2139666f420582f04e628a1e1225b4ecf49",
            "w": "036066a84d6ab109fd53ee769e7bbb89daf191d883007c5625a9b762c542108b86",
            "n": "0000002403006822",
            "d": 2.980776817797659e+64
          },
          "adProofsId": "73ecd680852af62f3ea673ad4c7a6bb860ccd05a4d6829c1d45f5ad2687fbdaf",
          "transactionsId": "a124874e6103fb70d44b247856695916b93dfb564604fbf142ce5e5bdbc1c914",
          "parentId": "875aaa0886c229607b3da2440f9cdb12f61ed2a0e56c6a9dd9536ac11079ff03"
        }
        |]
