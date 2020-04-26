{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import Data.ByteString as BS (readFile, ByteString, writeFile)
import Data.ByteString.UTF8 as UTF8 (fromString)
import System.Directory
import Control.Exception (catch, SomeException)
import Data.Map.Lazy as M (singleton, fromList, Map)
import Data.Foldable
import Template.Convert
import Data.List.Extra
import Template.Type
import Data.Maybe
import Data.ByteString.UTF8 (toString)
import Control.Monad (filterM)

getAllFiles :: FilePath ->  IO [FilePath]
getAllFiles root' = do
  let root = if last root' == '/' then root' else root' <> "/"
  roots' <- filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents root
  let roots = (root <>) <$> roots'
  dirs <- filterM doesDirectoryExist roots
  files <- filterM doesFileExist roots
  filesInDirs <- if null dirs then pure [] else mconcat <$> traverse getAllFiles dirs
  return (files <> filesInDirs)

addListLayer :: ByteString -> ObjectTree -> ObjectTree
addListLayer str ot = ObjNode (M.singleton str ot)

addLayer :: ByteString -> Map ByteString ObjectTree -> ObjectTree -> ObjectTree
addLayer str glbRes ot = ObjNode (M.singleton str ot <> glbRes)


getLayoutFile :: FilePath -> ObjectTree -> Maybe FilePath
getLayoutFile root x = case getNode "this" x >>= getLeaf "template" of
                    Just (ObjLeaf t) -> Just (root <> "theme/layout/" <> toString t <> ".html")
                    _ -> Nothing

trans :: FilePath -> IO ()
trans root' = do
  let root = if last root' == '/' then root' else root' <> "/"
  contentFiles <- getAllFiles root

  let posts = filter (\p -> (root <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
      statics = filter (\p -> (root <> "theme/static") `isPrefixOf` p) contentFiles
      partials = filter (\p -> (root <> "theme/layout/partial") `isPrefixOf` p) contentFiles


  rawPartials <- traverse BS.readFile partials
  let partialMap = zip (fmap (fromString . takeWhileEnd (/= '/')) partials) (ObjLeaf <$> rawPartials)
  let partialRes = singleton "partial" (ObjNode (M.fromList partialMap))
      globalRes = singleton "global" (ObjNode partialRes)

  let addGlb = addLayer "this" globalRes

  -- Remove out-dated public dir.
  ext <- doesDirectoryExist (root <> "public")
  _ <- if ext then removeDirectoryRecursive (root <> "public") else pure ()

  -- Copy static files of theme.
  traverse_ (\sPath ->
    let filePath = (((root <> "public") <>) . drop (length (root <> "theme/static"))) sPath in
    catch
      (copyFile sPath filePath) ((pure $
        createDirectoryIfMissing True (dropWhileEnd (/='/') filePath) >>
          copyFile sPath filePath) :: SomeException -> IO ()))
            statics

  -- Convert template.
  postObjs <- catMaybes <$> traverse parsePost posts
  postHtmls <- traverse (\x -> convertTP x <$> BS.readFile (fromJust $ getLayoutFile root x))
                 (addGlb <$> postObjs)
  indexHtml <- do
    let indexObjTree = addGlb $ addListLayer "post" $ ObjListNode $ (\(ObjNode x) -> x) <$> postObjs
    convertTP indexObjTree <$> BS.readFile (root <> "theme/layout/index.html")

  -- Generate index html.
  BS.writeFile (root <> "public/index.html") indexHtml

  -- Generate post html.
  traverse_ (\(sPath, mdHtml) -> do
    let htmlPath = ((root <> "public/post/") <>) . drop (length (root <> "content/post/")) . take (length sPath - 3) $ sPath
    _ <- createDirectoryIfMissing True htmlPath
    BS.writeFile (htmlPath <> "/index.html") mdHtml) (zip posts postHtmls)
