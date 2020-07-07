{-# LANGUAGE OverloadedStrings #-}
module Data.Template where

import Data.Map.Lazy              as M
import Data.ByteString            as BS (ByteString, readFile, init, last)
import Data.ByteString.UTF8             (fromString, toString)
import Data.ByteString.Search           (breakAfter)
import Data.Attoparsec.ByteString
import Data.Maybe
import Data.List.Extra            as LE (init, dropWhileEnd)
import Data.Time

import Data.Template.Type
import Data.Template.Parser

parsePost :: FilePath -> IO (Maybe ObjectTree)
parsePost path = do
  s <- BS.readFile path
  case parseOnly post s of
    Right (ObjNode x) -> do
      let relPath = snd $ breakAfter "content/" $ fromString $ LE.init $ dropWhileEnd (/='.') path
      pure $ Just (ObjNode (M.singleton "relLink" (ObjLeaf (relPath <> "/")) <> x))
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

-- Converting template to the exact html.

convertPartial :: ObjectTree -> Stmt -> Maybe ByteString
convertPartial objs (PartialStmt partPath) =
  case getNode "global" objs >>= getNode "partial" >>= getLeaf' partPath of
    Just partFile -> Just $ convertTP objs partFile
    _ -> Nothing
convertPartial _ _ = Nothing

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

getNode' :: ByteString -> ObjectTree -> Maybe (Map ByteString ObjectTree)
getNode' key obj = case getNode key obj of
                     Just (ObjNode x) -> Just x
                     _ -> Nothing

getLeaf' :: ByteString -> ObjectTree -> Maybe ByteString
getLeaf' key obj = case getLeaf key obj of
                     Just (ObjLeaf x) -> Just x
                     _ -> Nothing

singletonObjNode :: [ByteString] -> Map ByteString ObjectTree -> Map ByteString ObjectTree
singletonObjNode key x = (go key) x
  where go [] = id
        go (y:ys) = (singleton y) . ObjNode . (go ys)

addListLayer :: ByteString -> ObjectTree -> ObjectTree
addListLayer str ot = ObjNode (M.singleton str ot)

addLayer :: ByteString -> Map ByteString ObjectTree -> ObjectTree -> ObjectTree
addLayer str glbRes ot = ObjNode (M.singleton str ot <> glbRes)

toNodeList :: [ObjectTree] -> ObjectTree
toNodeList = ObjListNode . fmap (\(ObjNode x) -> x)

getLayoutFile :: FilePath -> ObjectTree -> FilePath
getLayoutFile root x = case getNode "this" x >>= getLeaf' "template" of
                         Just t -> (root <> "theme/layout/" <> toString t <> ".html")
                         _ -> "theme/layout/nolayout.html"

getDate :: ObjectTree -> UTCTime
getDate (ObjNode x) =
  case x !? "date" of
    Just (ObjLeaf x') -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" (toString x') :: UTCTime
    _                 -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2000-01-01" :: UTCTime
getDate _ = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2000-01-01" :: UTCTime

getCategory :: ObjectTree -> ByteString
getCategory (ObjNode x) = case x !? "category" of
                            Just (ObjLeaf x') -> x'
                            _ -> "Uncategoried"
getCategory _ = "Uncategoried"
