{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import System.Directory
import System.IO.Error              (isDoesNotExistError)
import Data.ByteString      as BS   (readFile, ByteString, writeFile)
import Data.ByteString.UTF8 as UTF8 (fromString, toString)
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
import Utils.Logging
import Utils.SitemapGenerator

getGlbRes :: FilePath -> [FilePath] -> IO (Map ByteString ObjectTree)
getGlbRes root allFiles = do
  let partialsRes = filter (isPrefixOf (root <> "theme/layout/partial")) allFiles
  partials <- M.singleton "partials" . ObjNode <$> getPartials partialsRes
  config <- M.singleton "config" . toObjectTree <$> parseConfig "config.toml"
  pure $ M.singleton "global" $ ObjNode (config <> partials)
  where getPartials ps = do
         rawPartials <- traverse BS.readFile ps
         pure $ fromList $ zip (fmap (fromString . takeWhileEnd (/= '/')) ps) (ObjLeaf <$> rawPartials)

parseConfig :: FilePath -> IO Config
parseConfig path = do
  tomlRes <- Toml.decodeFileEither configCodec path
  case tomlRes of
    Left errs -> logErrAndTerminate "Parsing config" (show errs)
    Right config -> pure config

gnrtPublic :: FilePath -> IO ()
gnrtPublic root' = do
  let root = if last root' == '/' then root' else root' <> "/"

  allFiles <- getAllFiles root
  let posts = filter (\p -> (root <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) allFiles
      pages = filter (\p -> (root <> "content/page/") `isPrefixOf` p && ".md" `isSuffixOf` p) allFiles
      cStatics = filter (isPrefixOf (root <> "content/static/")) allFiles
      statics  = filter (isPrefixOf (root <> "theme/static/")) allFiles

  glbRes <- getGlbRes root allFiles
  config <- parseConfig "config.toml"

  postObjs <- sortOn (Down . getDate) . catMaybes <$> traverse parsePost posts
  pageObjs <- catMaybes <$> traverse parsePost pages

  --- Convert index.
  let cates = fmap (\x -> mconcat [ M.singleton "cateName" (ObjLeaf x)
                                  , M.singleton "posts" (toNodeList (filter ((== Just x).getCategory) postObjs))]) $
                   catMaybes (nub $ getCategory <$> postObjs)
  indexHtml <- do
    let indexObjTree = addGlb glbRes $ ObjNode $ mconcat [ M.singleton "posts"       (toNodeList postObjs)
                                                         , M.singleton "categories"  (ObjListNode cates)]
    convertTP indexObjTree <$> BS.readFile (root <> "theme/layout/index.html")

  -- Remove out-dated public dir.
  _ <- removeDirContent (root <> "public/")

  -- Copy static files.
  copyFiles root "theme/static"   statics
  copyFiles root "content/static" cStatics

  let articles = (addGlb glbRes) <$> (postObjs <> pageObjs)
  -- Generate htmls.
  BS.writeFile (root <> "public/index.html") indexHtml  -- Index
  gnrtHtmls root articles          -- Posts and pages
  gnrtSitemap root (siteUrl config) articles


gnrtHtmls :: FilePath -> [ObjectTree] -> IO ()
gnrtHtmls root =
  traverse_ (\x -> do
    html <- convertTP x <$> BS.readFile (getLayoutFile root x)
    let relLink = ((<>) (root <> "public/")) . toString <$> (getNode "this" x >>= getLeaf' "relLink")
    traverse_ (createDirectoryIfMissing True) relLink
    traverse_ (`BS.writeFile` html) $ (<> "/index.html") <$> relLink)

gnrtSitemap :: FilePath -> ByteString -> [ObjectTree] -> IO ()
gnrtSitemap root site objs = do
  sitemap <- getSitemap site objs
  BS.writeFile (root <> "public/sitemap.xml") sitemap


copyFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFiles root prefix2Remove =
  traverse_ (\source -> copyFile' source (getTarget source))
  where
    copyFile' source target = catchJust (guard . isDoesNotExistError)
                                        (copyFile source target)
                                        (\_ -> cr8Dir target >> (copyFile source target))
    getTarget = ((root <> "public") <>) . drop (length (root <> prefix2Remove))
    cr8Dir path = createDirectoryIfMissing True (dropWhileEnd (/='/') path)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles root' = do
  -- Prevent it from walking into "..", "." dirs
  contents <- filter (not . (isPrefixOf ".")) <$> getDirectoryContents root'
  let root = if last root' == '/' then root' else root' <> "/"
      filesAndDirs = (root <>) <$> contents

  dirs <- filterM doesDirectoryExist filesAndDirs
  subFiles <- if null dirs then pure [] else mconcat <$> traverse getAllFiles dirs

  let files = filter (not . (`elem` dirs)) filesAndDirs
  return (files <> subFiles)

-- Remove anything except filename started with "." like ".git"
removeDirContent :: FilePath -> IO ()
removeDirContent root' = do
  ext <- doesDirectoryExist root'
  if ext
     then do
          contents <- filter (not . (isPrefixOf ".")) <$> getDirectoryContents root'
          let root = if last root' == '/' then root' else root' <> "/"
              filesAndDirs = (root <>) <$> contents
          dirs <- filterM doesDirectoryExist filesAndDirs
          let files = filter (not . (`elem` dirs)) filesAndDirs
          traverse_ removeDirectoryRecursive dirs
          traverse_ removeFile files
     else pure ()
