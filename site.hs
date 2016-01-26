{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.List (isInfixOf)
import Data.Monoid
import Text.Regex
import System.Directory
import System.FilePath.Posix

import Hakyll
import Hakyll.Web.Sass

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/*.scss" $ do
    route $ setExtension "css"
    compile $ fmap compressCss <$> (getResourceBody >>= renderSass)

  match "components/**" $ do
    route idRoute
    compile copyFileCompiler

  match "posts/dev/*" $ do
    route articleRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"  postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= processUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      devPosts <- recentFirst =<< loadAll "posts/dev/*"
      let indexCtx =
            listField "posts" postCtx (return devPosts) <>
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= processUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext

processUrls = relativizeUrls >=> removeIndexHtml

--
articleRoute :: Routes
articleRoute = customRoute makeR
    where
        makeR i  = shorten (toFilePath i) </> fileName (toFilePath i) </> "index.html"

        fileName :: FilePath -> FilePath
        fileName p = case (convertArticleFile . takeBaseName) p of
          Just np -> np
          Nothing -> error $ "[ERROR] wrong format: " ++ p

        shorten    = joinPath . tail . splitPath . takeDirectory

-- Removes date part from article file name.
convertArticleFile :: String -> Maybe String
convertArticleFile f = fmap last $ matchRegex articleRx f

articleRx :: Regex
articleRx = mkRegex "^([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})\\-(.+)$"

-- Replace url of the form foo/bar/index.html by foo/bar.
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
  removeIndexStr :: String -> String
  removeIndexStr url = case splitFileName url of
    (dir, "index.html") | isLocal dir -> dir
    _                                 -> url

  isLocal :: String -> Bool
  isLocal uri        = not (isInfixOf "://" uri)
