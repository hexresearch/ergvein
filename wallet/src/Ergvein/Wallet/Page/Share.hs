{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Share(
    sharePage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Clipboard (clipboardCopy)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Share
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Share (shareShareUrl)
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58
import Network.Haskoin.Keys

sharePage :: MonadFront t m => Currency -> m ()
sharePage cur = do
  let thisWidget = Just $ pure $ sharePage cur
  menuWidget (ShareTitle cur) thisWidget
  wrapper False $ divClass "share-content" $ do
    pks :: PublicKeystore <- getPublicKeystore
    let xPubKeyMb  = (egvXPubKey . egvPubKeyсhain'master) <$> M.lookup cur pks
        addressMb  = xPubAddr <$> xPubKeyMb
    maybe errorPage renderPage addressMb
    pure ()
  where
    errorPage :: MonadFront t m => m ()
    errorPage = do
      pure ()

    renderPage :: MonadFront t m => Address -> m ()
    renderPage addr = do
      let addrBase   = addrToString (getCurrencyNetwork cur) addr
      let shareAddr  = addrBase
          shareMoney = Money cur 1
      let tempUrl = generateURL shareAddr shareMoney
      textLabel ShareLink $ text tempUrl
      copyE    <- fmap (shareAddr  <$) $ outlineButton ShareCopy
      el "br" blank
      shareE   <- fmap (tempUrl    <$) $ outlineButton ShareShare
      el "br" blank
      shareQrE <- fmap (testBase64 <$) $ outlineButton ShareQR
      _ <- clipboardCopy copyE
      _ <- shareShareUrl shareE
      _ <- shareShareUrl shareQrE
      pure ()

    generateURL :: Base58 -> Money -> Text
    generateURL addrB58 money = case cur of
      --BTC   -> "bitcoin://" <> addrB58 <> "?amount=" <> (showMoneyUnit money units)
      BTC   -> "bitcoin://" <> addrB58
      -- TODO: Fix URL for Ergo
      ERGO  -> ""

    units :: Units
    units = Units {
        unitBTC  = Just BtcWhole
      , unitERGO = Just ErgWhole
      }

testBase64 :: Text
testBase64 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAABb2lDQ1BpY2MAACiRdZE7SwNBFIW/RCXigxRaqKRIEcUiQlAQS42FTRCJEYzaJJtsIuSx7CaI2Ao2FgEL0cZX4T/QVrBVEARFELHwF/hqJKx33ECCJLPM3o8zcy4zZ8AdyWl5qz0E+ULJjM6F/cvxFb/nDTeDeGjHl9AsY2ZhIULL8f2AS9X7MdWr9b6mozuVtjRwdQpPaoZZEp4WjmyUDMW7wv1aNpESPhYOmnJA4RulJx1+VZxx+FOxGYvOglv19GcaONnAWtbMC48KB/K5slY7j7pJT7qwtCh1SKYPiyhzhPGTpMw6OUqMSS1IZs19oT/fPEXxaPI32MQUR4aseIOilqVrWqouelq+HJsq9/95WvrEuNO9JwwdL7b9MQyePahWbPvnxLarp9D2DFeFur8oOU19iV6pa4Ej8G7DxXVdS+7D5Q4MPBkJM/Entcl06zq8n0NvHPruoGvVyaq2ztkjxLbkiW7h4BBGZL937ReXxWfa+r1HsAAAAAlwSFlzAAAPYQAAD2EBqD+naQAAAUlJREFUWEftl7sRgzAMhsGkoGcGNqBkELagYCt2YRMGoIIgX+AUx0IPwpEivuMobOn/0MtHuqwruXG5G7W99B/gtyOQpunlJcJG4GoIFgBCcBYC7CkfhwB4RFghODs2AmcgsDg171gASIEFQiKuGkQaCKm4CiAWiaqqPtpUI+4L3HIZYZGiKJJxHD2IVtwMEIrleZ5M07RHQ3PBmiKwKTnn3go0TJNkjIq6gHI0z/PbFgBpl94CKYRDBoC0EGYAquAg/9z0w1EyAcTEw8KTQqgBjlrNAqECwPmlWk0LIQbALcf1uQZCBJBl2d7vfd+LOo2DxFMLpjG51pzDf4N/uq47PBvbHIZhaduWtIMvozdfwiDeNI1aXGJAAmxfDe+6riW+TGeiAFi8LEuTY6lR9DKyXKuiyowcesQMxRVsVUV2ojb8gg7p4naAJ2zQRmOjezLqAAAAAElFTkSuQmCC"

textLabel :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> m a -- ^ Value
  -> m ()
textLabel lbl val = do
  i <- genId
  label i $ localizedText lbl
  elAttr "div" [("class","share-block-value"),("id",i)] $ val
  pure ()
