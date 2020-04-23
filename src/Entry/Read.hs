{-# LANGUAGE OverloadedStrings #-}
module Entry.Read where

import Data.ByteString as BS (ByteString, readFile, putStrLn)
import System.Directory
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Lazy ((!), Map)
import Template.Convert
import Template.Type
import Template.Parser (post)
import Data.Maybe
import Data.ByteString.UTF8 (toString)
import System.IO
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
  contentFiles <- filter ("./content/" `isPrefixOf`) <$> getAllFiles "."
  themeFiles <- filter ("./theme/" `isPrefixOf`) <$> getAllFiles "."
  let postMdFiles = filter (\p -> "./content/post/" `isPrefixOf` p && ".md" `isSuffixOf` p) contentFiles
      layoutFiles = filter (\p -> "./theme/layout/" `isPrefixOf` p && ".html" `isSuffixOf` p) themeFiles
  postObjs <- catMaybes <$> traverse parsePost postMdFiles
  tps <- traverse (\x -> convertTP x <$> BS.readFile (fromJust $ getLayoutFile x)) postObjs
  traverse BS.putStrLn tps
  return ()

getLayoutFile :: ObjectTree -> Maybe FilePath
getLayoutFile x = case x of
                    ObjNode node -> case node ! "template" of
                                      ObjLeaf y -> Just $ toString ("./theme/layout/" <> y <> ".html")
                                      _ -> Nothing
                    _ -> Nothing

getFromObjTree :: ObjectTree -> Map ByteString ObjectTree
getFromObjTree x = case x of
                     ObjLeaf _ -> mempty
                     ObjNode x' -> x'
