--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/highlight/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["docs.html"] $ do
      route idRoute
      compile $ do
        let docsCtx =
              constField "docspage" "" `mappend`
              defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/docs.html" docsCtx
            >>= loadAndApplyTemplate "templates/default.html" docsCtx
            >>= relativizeUrls

    create ["download.html"] $ do
      route idRoute
      compile $ do
        let docsCtx =
              constField "downpage" "" `mappend`
              defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/download.html" docsCtx
            >>= loadAndApplyTemplate "templates/default.html" docsCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home" `mappend`
                    constField "homepage" "" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
    match "features/*" $ compile templateBodyCompiler
