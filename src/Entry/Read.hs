{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import Data.ByteString as BS (readFile, ByteString, writeFile)
import Data.ByteString.UTF8 as UTF8 (fromString)
import Control.Exception (catch, SomeException)
import Data.Map.Lazy as M (singleton, fromList, Map, (!))
import Data.Foldable
import Data.Ord
import Template.Convert
import Data.List.Extra
import Data.Tuple.Extra
import System.Directory
import Template.Type
import Data.Maybe
import Data.ByteString.UTF8 (toString)
import Control.Monad (filterM)
import Data.Time

getAllFiles :: FilePath -> IO [FilePath]
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

getDate (ObjNode x) =
  case x ! "date" of
    ObjLeaf x' -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" (toString x') :: UTCTime
    _ -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2000-01-01" :: UTCTime

getCategory (ObjNode x) =
  case x ! "category" of
    ObjLeaf x' -> x'
    _ -> "Uncategoried"

toObjNodeList :: [ObjectTree] -> ObjectTree
toObjNodeList = ObjListNode . fmap (\(ObjNode x) -> x)

trans :: FilePath -> IO ()
trans root' = do
  let root = if last root' == '/' then root' else root' <> "/"
  contentFiles <- getAllFiles root

  let posts = filter (\p -> (root <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
      pages = filter (\p -> (root <> "content/page/") `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
      statics = filter (\p -> (root <> "theme/static") `isPrefixOf` p) contentFiles
      partials = filter (\p -> (root <> "theme/layout/partial") `isPrefixOf` p) contentFiles


  rawPartials <- traverse BS.readFile partials
  let partialMap = zip (fmap (fromString . takeWhileEnd (/= '/')) partials) (ObjLeaf <$> rawPartials)
      partialRes = singleton "partial" (ObjNode (M.fromList partialMap))
      globalRes = singleton "global" (ObjNode partialRes)

  let addGlb = addLayer "this" globalRes

  -- Convert template.
  let getHtml x = convertTP x <$> BS.readFile (fromJust $ getLayoutFile root x)
  --- Convert post.
  postObjs  <- sortOn (Down . getDate) . catMaybes <$> traverse parsePost posts
  postHtmls <- traverse (\x -> do
    html <- getHtml x
    let relLink = case getNode "this" x >>= getLeaf "relLink" of
                    Just (ObjLeaf x) -> toString x
                    Nothing -> "/404.html"
    pure (relLink, html))
                 (fmap addGlb postObjs)

  --- Convert page.
  pageObjs <- catMaybes <$> traverse parsePost pages
  pageHtmls <- traverse (\x -> convertTP x <$> BS.readFile (fromJust $ getLayoutFile root x))
                 (addGlb <$> pageObjs)

  --- Convert index.
  let categories = nub $ getCategory <$> postObjs
      cates = fmap (\x -> mconcat [ M.singleton "cateName" (ObjLeaf x)
                                  , M.singleton "post" $ toObjNodeList (filter ((x==) .getCategory) postObjs)]) categories
  indexHtml <- do
    let indexObjTree = addGlb $ ObjNode $ mconcat [ M.singleton "post" $ toObjNodeList postObjs
                                                  , M.singleton "categories" (ObjListNode cates)]
    convertTP indexObjTree <$> BS.readFile (root <> "theme/layout/index.html")


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

  -- Generate index html.
  BS.writeFile (root <> "public/index.html") indexHtml

  -- Generate post html.
  traverse_ (\(sPath, mdHtml) -> do
    let htmlPath = ((root <> "public") <>) sPath
    _ <- createDirectoryIfMissing True htmlPath
    BS.writeFile (htmlPath <> "/index.html") mdHtml) postHtmls

  -- Generate page html.
  traverse_ (\(sPath, mdHtml) -> do
    let htmlPath = ((root <> "public/") <>) . drop (length (root <> "content/page/")) . take (length sPath - 3) $ sPath
    _ <- createDirectoryIfMissing True htmlPath
    BS.writeFile (htmlPath <> "/index.html") mdHtml) (zip pages pageHtmls)
