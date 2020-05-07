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
import System.Directory
import Template.Type
import Data.Maybe
import Data.ByteString.UTF8 (toString)
import Control.Monad (filterM)
import Data.Time

trans :: FilePath -> IO ()
trans root' = do
  let root = if last root' == '/' then root' else root' <> "/"
      rootPublic = root <> "public/"
  allFiles <- getAllFiles root

  let posts = filter (\p -> (root <> "content/post/") `isPrefixOf` p && ".md" `isSuffixOf` p) allFiles
      pages = filter (\p -> (root <> "content/page/") `isPrefixOf` p && ".md" `isSuffixOf` p) allFiles
      cStatics = filter (isPrefixOf (root <> "content/static/")) allFiles
      statics  = filter (isPrefixOf (root <> "theme/static/")) allFiles
      partials = filter (isPrefixOf (root <> "theme/layout/partial")) allFiles

  globalRes <- do
    rawPartials <- traverse BS.readFile partials
    let partialMap = zip (fmap (fromString . takeWhileEnd (/= '/')) partials) (ObjLeaf <$> rawPartials)
        partialRes = singleton "partial" (ObjNode (M.fromList partialMap))
    pure (singleton "global" (ObjNode partialRes))

  let addGlb = addLayer "this" globalRes

  -- Convert template.
  let getHtml x = convertTP x <$> BS.readFile (fromJust $ getLayoutFile root x)
      convertTemplate x = do
            html <- getHtml x
            let relLink = case getNode "this" x >>= getLeaf "relLink" of
                            Just (ObjLeaf x') -> toString x'
                            _ -> "/404.html"
            pure (relLink, html)

  --- Convert post.
  postObjs  <- sortOn (Down . getDate) . catMaybes <$> traverse parsePost posts
  postHtmls <- traverse (convertTemplate.addGlb) postObjs

  --- Convert page.
  pageObjs <- catMaybes <$> traverse parsePost pages
  pageHtmls <- traverse (convertTemplate.addGlb) pageObjs

  --- Convert index.
  let categories = nub $ getCategory <$> postObjs
      cates = fmap (\x -> mconcat [ M.singleton "cateName" (ObjLeaf x)
                                  , M.singleton "post" $ toNodeList (filter ((x==) .getCategory) postObjs)]) categories
  indexHtml <- do
    let indexObjTree = addGlb $ ObjNode $ mconcat [ M.singleton "post" $ toNodeList postObjs
                                                  , M.singleton "categories" (ObjListNode cates)]
    convertTP indexObjTree <$> BS.readFile (root <> "theme/layout/index.html")


  -- Remove out-dated public dir.
  ext <- doesDirectoryExist rootPublic
  _ <- if ext then removeDirectoryRecursive (root <> "public") else pure ()

  -- Copy static files.
  copyStatics root "theme/static" statics
  copyStatics root "content/static" cStatics

  -- Generate htmls.
  BS.writeFile (rootPublic <> "index.html") indexHtml  -- Index
  gnrtHtmls rootPublic id postHtmls  -- Posts
  gnrtHtmls rootPublic id pageHtmls -- Pages
    where getPagePath root x = drop (length (root <> x)) . init . takeWhileEnd (/= '.')

gnrtHtmls :: String -> (String -> String) -> [(String, ByteString)] -> IO ()
gnrtHtmls rootPublic predicate =
  traverse_ (\(sPath, mdHtml) -> do
    let htmlPath = (rootPublic <>) . predicate $ sPath
    _ <- createDirectoryIfMissing True htmlPath
    BS.writeFile (htmlPath <> "/index.html") mdHtml)

copyStatics root path =
  traverse_ (\sPath ->
    let filePath = (((root <> "public") <>) . drop (length (root <> path))) sPath
        copyFunc = copyFile sPath filePath
        cr8Dir = createDirectoryIfMissing True (dropWhileEnd (/='/') filePath) in
    catch copyFunc ((\_ -> cr8Dir >> copyFunc) :: SomeException -> IO ()))

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

getDate :: ObjectTree -> UTCTime
getDate (ObjNode x) =
  case x ! "date" of
    ObjLeaf x' -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" (toString x') :: UTCTime
    _ -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2000-01-01" :: UTCTime
getDate _ = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2000-01-01" :: UTCTime

getCategory :: ObjectTree -> ByteString
getCategory (ObjNode x) =
  case x ! "category" of
    ObjLeaf x' -> x'
    _ -> "Uncategoried"
getCategory _ = "Uncategoried"

toNodeList :: [ObjectTree] -> ObjectTree
toNodeList = ObjListNode . fmap (\(ObjNode x) -> x)
