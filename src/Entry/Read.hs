{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import Data.ByteString as BS (readFile, putStrLn, ByteString, writeFile)
import System.Directory
import Control.Exception (catch, SomeException)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Lazy as M ((!), singleton)
import Data.Foldable
import Template.Convert
import Data.List.Extra
import Template.Type
import Data.Maybe
import Data.ByteString.UTF8 (toString)
import Control.Monad (filterM)

getAllFiles :: FilePath ->  IO [FilePath]
getAllFiles path' = do
  let path = if last path' == '/' then path' else path' <> "/"
  paths' <- filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents path
  let paths = (path <>) <$> paths'
  dirs <- filterM doesDirectoryExist paths
  files <- filterM doesFileExist paths
  filesInDirs <- if null dirs then pure [] else mconcat <$> traverse getAllFiles dirs
  return (files <> filesInDirs)

trans :: FilePath -> IO ()
trans path' = do
  let path = if last path' == '/' then path' else path' <> "/"
  -- contentFiles <- filter ((path <> "content/") `isPrefixOf`) <$> getAllFiles "."
  contentFiles <- getAllFiles path

  let postMdFiles = filter (\p -> (path <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
      statics = filter (\p -> (path <> "theme/static") `isPrefixOf` p) contentFiles

  postObjs <- catMaybes <$> traverse parsePost postMdFiles
  postHtmls <- traverse (\x -> convertTP x <$> BS.readFile (fromJust $ getLayoutFile x)) postObjs
  indexHtml <- (\x -> convertTP x <$> BS.readFile (path <> "theme/layout/index.html"))
                  (addListLayer "post" $ ObjListNode $ (\(ObjNode x) -> x) <$> postObjs)

  -- Remove out-dated public dir.
  ext <- doesDirectoryExist (path <> "public")
  _ <- if ext then pure () else removeDirectoryRecursive (path <> "public")

  -- Copy static files of theme.
  traverse_ (\sPath ->
    let filePath = (((path <> "public") <>) . drop (length (path <> "theme/static"))) sPath in
    catch
      (copyFile sPath filePath) ((\_ ->
        createDirectoryIfMissing True (init . dropWhileEnd (/='.') $ filePath) >>
          copyFile sPath filePath) :: SomeException -> IO ()))
            statics

  -- Generate index html.
  BS.writeFile (path <> "public/index.html") indexHtml

  -- Generate post html.
  traverse_ (\(sPath, mdHtml) -> do
    let htmlPath = ((path <> "public/post/") <>) . drop (length (path <> "content/post/")) . take (length sPath - 3) $ sPath
    _ <- createDirectoryIfMissing True htmlPath
    BS.writeFile (htmlPath <> "/index.html") mdHtml) (zip postMdFiles postHtmls)

addListLayer :: ByteString -> ObjectTree -> ObjectTree
addListLayer str ot = ObjNode $ M.singleton str ot

getLayoutFile :: ObjectTree -> Maybe FilePath
getLayoutFile x = case x of
                    ObjNode node -> case node ! "template" of
                                      ObjLeaf y -> Just $ toString ("./test-data/theme/layout/" <> y <> ".html")
                                      _ -> Nothing
                    _ -> Nothing
