{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Control.Monad
import Data.List (isInfixOf)
import Data.Monoid
import Text.Regex
import System.Directory
import System.FilePath.Posix

import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "bower_components/jquery/dist/**" $ do
    route $ gsubRoute "bower_components/jquery/dist/" (const "js/")
    compile copyFileCompiler

  match "bower_components/bootstrap/dist/**" $ do
    route $ gsubRoute "bower_components/bootstrap/dist/" (const "")
    compile copyFileCompiler

  match "posts/dev/*" $ do
    route articleRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"  postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= processUrls

  create ["archive/index.html"] $ do
    route idRoute
    compile $ do
      devPosts <- recentFirst =<< loadAll "posts/dev/*"
      let archiveCtx =
            listField "posts" postCtx (return devPosts) <>
            constField "title" "Archives" <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= processUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      devPosts <- recentFirst =<< loadAll "posts/dev/*"
      let indexCtx =
            listField "posts" postCtx (return devPosts) <>
            constField "title" "Home" <>
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
