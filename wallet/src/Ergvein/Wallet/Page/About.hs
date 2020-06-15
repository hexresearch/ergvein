{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.About(
    aboutPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.About
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

aboutPage :: MonadFront t m => m ()
aboutPage = wrapper AboutTitle (Just $ pure aboutPage) $ do
  h3 $ localizedText $ AboutTitle
  elAttr "hr" [("class","about-hr-sep")] blank
  divClass "about-wrapper" $ do
    aboutContent $ do
      aboutRow $ do
        aboutCellLabel $ localizedText AboutVersion
        aboutCellValue $ text "0.0.1"
      aboutRow $ do
        aboutCellLabel $ localizedText AboutLicence
        aboutCellValue $ text "MIT License"
      aboutRow $ do
        aboutCellLabel $ localizedText AboutHomepage
        let homepageUrl = "https://cypra.io"
        aboutCellValue $ hyperlink "link" homepageUrl homepageUrl
      aboutRow $ do
        aboutCellLabel $ localizedText AboutDevelopers
        aboutCellValue $ do
          text "One One"
          elBR
          text "Two Two"
          elBR
          text "Three Three"
    divClass "about-line" $ divClass "about-distrib" $ localizedText AboutDistrib
  pure ()
  where
    aboutContent   = divClass "about-line" . divClass "about-content"
    aboutRow       = divClass "about-content-row"
    aboutCellLabel = divClass "about-content-cell-label"
    aboutCellValue = divClass "about-content-cell-value"
    elBR           = el "br" blank
