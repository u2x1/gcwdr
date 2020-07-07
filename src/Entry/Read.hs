{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import System.Directory
import System.IO.Error              (isDoesNotExistError)
import Data.ByteString      as BS   (readFile, ByteString, writeFile)
import Data.ByteString.UTF8 as UTF8 (fromString, toString)
import Data.Map.Lazy        as M    (singleton, fromList, Map)
import Data.Foldable
import Data.Ord
import Data.List.Extra              (nub, isSuffixOf, isPrefixOf, takeWhileEnd, dropWhileEnd, sortOn)
import Data.Maybe
import Control.Monad                (filterM, guard)
import Control.Exception            (catchJust)

import Data.Template
import Data.Template.Type
import Utils.SitemapGenerator

gnrtPublic :: FilePath -> IO ()
gnrtPublic root' = do
  let root = if last root' == '/' then root' else root' <> "/"

  allFiles <- getAllFiles root
  let posts = filter (\p -> (root <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) allFiles
      pages = filter (\p -> (root <> "content/page/") `isPrefixOf` p && ".md" `isSuffixOf` p) allFiles
      cStatics = filter (isPrefixOf (root <> "content/static/")) allFiles
      statics  = filter (isPrefixOf (root <> "theme/static/")) allFiles
      partials = filter (isPrefixOf (root <> "theme/layout/partial")) allFiles

  glbRes <- getGlbRes partials
  let glbObjNode = (addLayer "this" glbRes) . ObjNode

  postObjs <- sortOn (Down . getDate) . catMaybes <$> traverse parsePost posts
  pageObjs <- catMaybes <$> traverse parsePost pages

  --- Convert index.
  let cates = fmap (\x -> mconcat [ M.singleton "cateName" (ObjLeaf x)
                                  , M.singleton "posts" (toNodeList (filter ((== x) .getCategory) postObjs))]) (nub $ getCategory <$> postObjs)
  indexHtml <- do
    let indexObjTree = glbObjNode $ mconcat [ M.singleton "posts"       (toNodeList postObjs)
                                            , M.singleton "categories"  (ObjListNode cates)]
    convertTP indexObjTree <$> BS.readFile (root <> "theme/layout/index.html")

  -- Remove out-dated public dir.
  _ <- removeDirContent (root <> "public/")

  -- Copy static files.
  copyFiles root "theme/static"   statics
  copyFiles root "content/static" cStatics

  let articles = (addLayer "this" glbRes) <$> (postObjs <> pageObjs)
  -- Generate htmls.
  BS.writeFile (root <> "public/index.html") indexHtml  -- Index
  gnrtHtmls root glbRes articles          -- Posts and pages
  gnrtSitemap root "" articles

  where
    getGlbRes partials = do
      -- Get partial files.
      rawPartials <- traverse BS.readFile partials
      let partialMap = fromList $ zip (fmap (fromString . takeWhileEnd (/= '/')) partials) (ObjLeaf <$> rawPartials)
      pure (singletonObjNode ["global", "partial"] partialMap)

gnrtHtmls :: FilePath -> Map ByteString ObjectTree -> [ObjectTree] -> IO ()
gnrtHtmls root glbRes =
  traverse_ ((\x -> do
    html <- convertTP x <$> BS.readFile (getLayoutFile root x)
    let relLink = ((<>) (root <> "public/")) $ case getNode "this" x >>= getLeaf' "relLink" of
                                      Just x' -> toString x'
                                      _ -> "/404.html"
    _ <- createDirectoryIfMissing True relLink
    BS.writeFile (relLink <> "/index.html") html))

gnrtSitemap :: FilePath -> ByteString -> [ObjectTree] -> IO ()
gnrtSitemap root site objs = do
  let infos = catMaybes $ getUrlInfo <$> objs
  let sitemap = packSitemap site infos
  BS.writeFile (root <> "public/sitemap.xml") sitemap

getUrlInfo :: ObjectTree -> Maybe URLInfo
getUrlInfo obj = URLInfo <$> l <*> m <*> p
  where l = getNode "this" obj >>= getLeaf' "relLink"
        m = getNode "this" obj >>= getLeaf' "date"
        p = Just 6


copyFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFiles root prefix2Remove =
  traverse_ (\source -> copyFile' source (getTarget source))
  where
    copyFile' source target = catchJust (guard . isDoesNotExistError)
                                        (copyFile source target)
                                        (\_ -> cr8Dir target >> (copyFile source target))
    getTarget = (((root <> "public") <>) . drop (length (root <> prefix2Remove)))
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
