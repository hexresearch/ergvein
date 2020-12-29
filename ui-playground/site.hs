--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "css/*"   $ route idRoute >> compile copyFileCompiler
  match "js/*"    $ route idRoute >> compile copyFileCompiler
  match "fonts/*" $ route idRoute >> compile copyFileCompiler
  match "index.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
  match "templates/*" $ compile templateCompiler
