{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import System.Directory
import System.IO.Error              (isDoesNotExistError)
import Data.Text                    (Text)
import qualified Data.Text  as T
import Data.Text.IO         as T    (readFile, writeFile)
import Data.Ord                     (Down(Down))
import Data.Maybe                   (catMaybes)
import Data.Map.Lazy        as M    (singleton, fromList, Map)
import Data.Foldable                (traverse_)
import Data.List.Extra              (nub, isSuffixOf, isPrefixOf, takeWhileEnd, dropWhileEnd, sortOn)
import Control.Monad                (filterM, guard)
import Control.Exception            (catchJust)

import qualified Toml

import Data.Markdown                (parsePost)
import Data.Template
import Data.Template.Type
import Data.Config
import Data.Config.Type
import Module.Console
import Utils.Logging
import Utils.SitemapGenerator

import System.FilePath

getGlbRes :: FilePath -> [FilePath] -> IO (Map Text ObjectTree)
getGlbRes themePath allFiles = do
  let partialsRes = filter (isPrefixOf (themePath </> "layout/partial")) allFiles
  partials <- M.singleton "partials" . ObjNode <$> getPartials partialsRes
  config <- M.singleton "config" . toObjectTree <$> parseConfig "config.toml"
  pure $ M.singleton "global" $ ObjNode (config <> partials)
  where getPartials ps = do
         rawPartials <- traverse T.readFile ps
         pure $ fromList $ zip (fmap (T.pack . takeWhileEnd (/= '/')) ps) (ObjLeaf <$> rawPartials)

parseConfig :: FilePath -> IO Config
parseConfig path = do
  tomlRes <- Toml.decodeFileEither configCodec path
  case tomlRes of
    Left errs -> logErrAndTerminate "Parsing config" (show errs)
    Right config -> pure config

gnrtPublic :: Config -> IO ()
gnrtPublic cfg = do
  logWT Info "generating output"
  let atclPath  = articleDir cfg
      themePath = themeDir cfg
  articleRes <- getAllFiles atclPath
  themeRes <- getAllFiles themePath
  let posts = filter (\p -> (atclPath </> "post/") `isPrefixOf` p && ".md" `isSuffixOf` p) articleRes 
      pages = filter (\p -> (atclPath </> "page/") `isPrefixOf` p && ".md" `isSuffixOf` p) articleRes
      cStatics = filter (isPrefixOf (atclPath </> "static/"))  articleRes 
      statics  = filter (isPrefixOf (themePath </> "static/")) themeRes

  glbRes <- getGlbRes themePath themeRes
  -- config <- parseConfig "config.toml"

  postObjs <- sortOn (Down . getDate) . catMaybes <$> traverse parsePost posts
  pageObjs <- catMaybes <$> traverse parsePost pages


  --- Convert index.
  let cates = fmap (\x -> mconcat [ M.singleton "cateName" (ObjLeaf x)
                                  , M.singleton "posts" (toNodeList (filter ((== Just x).getCategory) postObjs))]) $
                   catMaybes (nub $ getCategory <$> postObjs)
  indexHtml <- do
    let indexObjTree = addGlb glbRes $ ObjNode $ mconcat [ M.singleton "posts"       (toNodeList postObjs)
                                                         , M.singleton "categories"  (ObjNodeList cates)]
    let index = convertTP indexObjTree <$> T.readFile (themePath </> "layout/index.html")
    getFromTP "index" =<< index

  -- Remove out-dated public dir.
  _ <- removeDirContent (outputDir cfg)

  -- Copy static files.
  let outputPath = outputDir cfg
  copyFiles outputPath (themePath </> "static/")   statics
  copyFiles outputPath (atclPath  </> "static/") cStatics

  let articles = (addGlb glbRes) <$> runAtclModule (postObjs <> pageObjs)
  -- Generate htmls.
  T.writeFile ((outputDir cfg) </> "index.html") indexHtml  -- Index
  gnrtHtmls   outputPath themePath articles          -- Posts and pages
  gnrtSitemap outputPath (siteUrl cfg) articles


getFromTP :: String -> Either [String] a -> IO a
getFromTP obj (Left x) = logErrAndTerminate ("parsing " <> obj) (unlines x)
getFromTP _ (Right x) = pure x

getLayoutFile :: FilePath -> ObjectTree -> FilePath
getLayoutFile themePath x = case getNode "this" x >>= getLeaf' "template" of
                         Just t -> (themePath </> "layout" </> T.unpack t <> ".html")
                         _ -> themePath </> "layout/nolayout.html"

fromMaybeM :: String -> Maybe a -> IO a
fromMaybeM _ (Just x) = pure x 
fromMaybeM msg _ = logErrAndTerminate msg "encountered Nothing in fromMaybe"

gnrtHtmls :: FilePath -> FilePath -> [ObjectTree] -> IO ()
gnrtHtmls outputPath themePath =
  traverse_ (\x -> do
    title <- fromMaybeM "getting title from article" (T.unpack <$> (getNode "this" x >>= getLeaf' "title"))
    relLink <- fromMaybeM "getting relLink from article" ((outputPath </>) . T.unpack <$> (getNode "this" x >>= getLeaf' "relLink"))
    logWT Info $ "generating article \"" <> title <> "\""
    html <- getFromTP "articles" =<< (convertTP x <$> T.readFile (getLayoutFile themePath x))
    _ <- createDirectoryIfMissing True relLink
    T.writeFile (relLink </> "index.html") html)

gnrtSitemap :: FilePath -> Text -> [ObjectTree] -> IO ()
gnrtSitemap outputPath site objs = do
  let siteMapPath = outputPath </> "sitemap.xml"
  logWT Info $ "generating sitemap at " <> siteMapPath
  sitemap <- getSitemap site objs
  T.writeFile siteMapPath sitemap


copyFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFiles outputPath inputPath =
  traverse_ (\source -> copyFile' source (getTarget source))
  where
    copyFile' source target = do
        logWT Info $ "copying file " <> source <> " to " <> target
        catchJust (guard . isDoesNotExistError)
                  (copyFile source target)
                  (\_ -> cr8Dir target >> (copyFile source target))
    getTarget = (outputPath </>) . drop (length inputPath)
    cr8Dir path = createDirectoryIfMissing True (dropWhileEnd (/='/') path)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles root' = do
  -- Prevent it from walking into ".git", ".stack-work" dirs
  contents <- filter (not . (isPrefixOf ".")) <$> listDirectory root'
  let root = if last root' == '/' then root' else root' <> "/"
      filesAndDirs = (root </>) <$> contents

  dirs <- filterM doesDirectoryExist filesAndDirs
  subFiles <- if null dirs then pure [] else mconcat <$> traverse getAllFiles dirs

  let files = filter (not . (`elem` dirs)) filesAndDirs
  return (files <> subFiles)

-- Remove anything except filename started with "." like ".git"
removeDirContent :: FilePath -> IO ()
removeDirContent root = do
  logWT Info $ "removing dir content " <> root
  ext <- doesDirectoryExist root
  if ext
     then do
          contents <- filter (not . (isPrefixOf ".")) <$> getDirectoryContents root
          let filesAndDirs = (root </>) <$> contents
          dirs <- filterM doesDirectoryExist filesAndDirs
          let files = filter (not . (`elem` dirs)) filesAndDirs
          traverse_ removeDirectoryRecursive dirs
          traverse_ removeFile files
     else createDirectory root >> pure ()