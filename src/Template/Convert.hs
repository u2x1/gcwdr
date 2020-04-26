{-# LANGUAGE OverloadedStrings #-}
module Template.Convert where

import Template.Type
import Data.Map.Lazy as M
import Data.ByteString as BS (ByteString, readFile, init, last)
import Data.ByteString.UTF8 (fromString)
import Data.Attoparsec.ByteString
import Template.Parser
import Data.Maybe
import Data.List.Extra as LE (takeWhileEnd, init, dropWhileEnd)

parsePost :: FilePath -> IO (Maybe ObjectTree)
parsePost path = do
  s <- BS.readFile path
  case parseOnly post s of
    Right (ObjNode x) -> do
      let relPath = fromString $ takeWhileEnd (/='/') $ LE.init $ dropWhileEnd (/='.') path
      pure $ Just (ObjNode (M.singleton "relLink" (ObjLeaf ("/post/" <> relPath <> "/")) <> x))
    _ -> pure Nothing

convertTP :: ObjectTree -> ByteString -> ByteString
convertTP objTree s = case parseOnly (many' stmt) (s <> "\n") of
                Right [] -> ""
                Right xs -> concatAndInit $ convertTP' objTree <$> xs
                _ -> ""

concatAndInit :: [Maybe ByteString] -> ByteString
concatAndInit x = concatAndInit' $ catMaybes x

concatAndInit' :: [ByteString] -> ByteString
concatAndInit' x
  | mconcat x == "" = mconcat x
  | BS.last (mconcat x) == 10 = BS.init $ mconcat x
  | otherwise = mconcat x

convertTP' :: ObjectTree -> Stmt -> Maybe ByteString
convertTP' _ (Raw rawHtml) = Just rawHtml
convertTP' _ IfStmt = Nothing
convertTP' objs s@ForeachStmt {} = convertForeach objs s
convertTP' objs s@DotStmt {} = convertDot objs s
convertTP' objs s@PartialStmt {} = convertPartial objs s

convertPartial :: ObjectTree -> Stmt -> Maybe ByteString
convertPartial objs (PartialStmt partPath) =
  case getNode "global" objs >>= getNode "partial" >>= getLeaf partPath of
    Just (ObjLeaf partFile) -> Just $ convertTP objs partFile
    _ -> Nothing
convertPartial _ _ = Nothing


getNode :: ByteString -> ObjectTree -> Maybe ObjectTree
getNode key (ObjNode objs) = case objs ! key of
                               x@(ObjNode _) -> Just x
                               _ -> Nothing
getNode _ _ = Nothing

getLeaf :: ByteString -> ObjectTree -> Maybe ObjectTree
getLeaf key (ObjNode objs) = case objs ! key of
                           x@(ObjLeaf _) -> Just x
                           _ -> Nothing
getLeaf _ _ = Nothing



convertForeach :: ObjectTree -> Stmt -> Maybe ByteString
convertForeach (ObjNode objs) (ForeachStmt holder dotObj stmts) =
  case convertDot2NodeList (ObjNode objs) dotObj of
    Just nodeList -> mconcat.mconcat $
      fmap (\node -> fmap (convertTP' (ObjNode $ M.singleton holder (ObjNode node))) stmts) nodeList
    _ -> Nothing
convertForeach _ _ = Nothing

convertDot2NodeList :: ObjectTree -> Stmt -> Maybe [Map ByteString ObjectTree]
convertDot2NodeList (ObjNode objs) (DotStmt obj mems) =
  let
    run _ (ObjListNode s) [] = Just s
    run objs' (ObjNode obj') (x:xs) = run objs' (obj' ! x) xs
    run _ _ _ = Nothing in
  if obj /= ""
     then run objs (objs ! obj) mems
     else run objs (ObjNode objs) mems
convertDot2NodeList _ _ = Nothing

convertDot :: ObjectTree -> Stmt -> Maybe ByteString
convertDot (ObjNode objs) (DotStmt obj mems) =
  let
    run (ObjLeaf s) [] = Just s
    run (ObjNode obj') (x:xs) = run (obj' ! x) xs
    run _ _ = Nothing in
  if obj /= ""
     then run (objs ! obj) mems
     else run (ObjNode objs) mems
convertDot _ _ = Nothing