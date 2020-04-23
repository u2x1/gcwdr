{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import Data.ByteString as BS (readFile, putStrLn, ByteString)
import System.Directory
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Lazy as M ((!), singleton)
import Template.Convert
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

trans :: IO ()
trans = do
  contentFiles <- filter ("./test-data/content/" `isPrefixOf`) <$> getAllFiles "."
  let postMdFiles = filter (\p -> "./test-data/content/post/" `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
  postObjs <- catMaybes <$> traverse parsePost postMdFiles
  tps <- traverse (\x -> convertTP x <$> BS.readFile (fromJust $ getLayoutFile x)) postObjs
  tps <- (\x -> convertTP x <$> BS.readFile "./test-data/theme/layout/index.html") (addLayer "post" $ ObjListNode $ (\(ObjNode x) -> x) <$> postObjs)
  BS.putStrLn tps
  return ()

addLayer :: ByteString -> ObjectTree -> ObjectTree
addLayer str ot = ObjNode $ M.singleton str ot

getLayoutFile :: ObjectTree -> Maybe FilePath
getLayoutFile x = case x of
                    ObjNode node -> case node ! "template" of
                                      ObjLeaf y -> Just $ toString ("./test-data/theme/layout/" <> y <> ".html")
                                      _ -> Nothing
                    _ -> Nothing
