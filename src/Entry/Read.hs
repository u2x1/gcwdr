{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import Data.Attoparsec.ByteString (parseOnly, many')
import Template.Parser
import System.IO

import Data.ByteString as BS (readFile, putStrLn, ByteString, writeFile, pack)
import Data.ByteString.UTF8 as UTF8 (fromString)
import System.Directory
import Control.Exception (catch, SomeException)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Lazy as M ((!), singleton, fromList, Map)
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

addListLayer :: ByteString -> ObjectTree -> ObjectTree
addListLayer str ot = ObjNode (M.singleton str ot)

addLayer :: ByteString -> Map ByteString ObjectTree -> ObjectTree -> ObjectTree
addLayer str glbRes ot = ObjNode (M.singleton str ot <> glbRes)

getLayoutFile :: ObjectTree -> Maybe FilePath
getLayoutFile x = case getNode "this" x >>= getLeaf "template" of
                    Just (ObjLeaf t) -> Just $ toString ("./test-data/theme/layout/" <> t <> ".html")
                    _ -> Nothing

trans :: FilePath -> IO ()
trans path' = do
  let path = if last path' == '/' then path' else path' <> "/"
  contentFiles <- getAllFiles path

  let postMdFiles = filter (\p -> (path <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
      statics = filter (\p -> (path <> "theme/static") `isPrefixOf` p) contentFiles
      partials = filter (\p -> (path <> "theme/layout/partial") `isPrefixOf` p) contentFiles


  rawPartials <- traverse BS.readFile partials
  let partialMap = zip (fmap (fromString . takeWhileEnd (/= '/')) partials) (ObjLeaf <$> rawPartials)
  let partialRes = singleton "partial" (ObjNode (M.fromList partialMap))
      globalRes = singleton "global" (ObjNode partialRes)

  -- Remove out-dated public dir.
  ext <- doesDirectoryExist (path <> "public")
  _ <- if ext then removeDirectoryRecursive (path <> "public") else pure ()

  -- Copy static files of theme.
  traverse_ (\sPath ->
    let filePath = (((path <> "public") <>) . drop (length (path <> "theme/static"))) sPath in
    catch
      (copyFile sPath filePath) ((\_ ->
        createDirectoryIfMissing True (init . dropWhileEnd (/='/') $ filePath) >>
          copyFile sPath filePath) :: SomeException -> IO ()))
            statics

  -- Convert template.
  postObjs <- catMaybes <$> traverse parsePost postMdFiles
  postHtmls <- traverse (\x -> convertTP x <$> BS.readFile (fromJust $ getLayoutFile x))
                 (addLayer "this" globalRes <$> postObjs)
  indexHtml <- do
    let indexObjTree = addLayer "this" globalRes $ addListLayer "post" $ ObjListNode $ (\(ObjNode x) -> x) <$> postObjs
    convertTP indexObjTree <$> BS.readFile (path <> "theme/layout/index.html")

  -- Generate index html.
  BS.writeFile (path <> "public/index.html") indexHtml

  -- Generate post html.
  traverse_ (\(sPath, mdHtml) -> do
    let htmlPath = ((path <> "public/post/") <>) . drop (length (path <> "content/post/")) . take (length sPath - 3) $ sPath
    _ <- createDirectoryIfMissing True htmlPath
    BS.writeFile (htmlPath <> "/index.html") mdHtml) (zip postMdFiles postHtmls)
