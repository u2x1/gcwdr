{-# LANGUAGE OverloadedStrings #-}
module Data.Template where

import Data.Map.Lazy              as M
import Data.Text                       (Text)
import Data.Text                  as T (unpack, init, last)
import Data.Attoparsec.Text
import Data.Either
import Data.Time

import Data.Template.Type
import Data.Template.Parser

convertTP :: ObjectTree -> Text -> Either [String] Text
convertTP objTree s = case parseOnly (many' stmt) (s <> "\n") of
                Right xs -> concatAndInit $ convertTP' objTree <$> xs
                _ -> Left ["failed to parse statements"]

concatAndInit :: [Either String Text] -> Either [String] Text
concatAndInit x = if Prelude.null $ lefts x
                     then Right $ concatAndInit' (rights x)
                     else Left $ lefts x

concatAndInit' :: [Text] -> Text
concatAndInit' x
  | mconcat x == "" = mconcat x
  | T.last (mconcat x) == '\n' = T.init $ mconcat x
  | otherwise = mconcat x

convertTP' :: ObjectTree -> Stmt -> Either String Text
convertTP' _    (Raw rawHtml)    = Right rawHtml
convertTP' _    IfStmt           = Left "Not implemented yet"
convertTP' objs s@ForeachStmt {} = convertForeach objs s
convertTP' objs s@DotStmt {}     = convertDot objs s
convertTP' objs s@PartialStmt {} = convertPartial objs s

-- Converting template to the actual html.

convertPartial :: ObjectTree -> Stmt -> Either String Text
convertPartial objs (PartialStmt partPath) =
  case getNode "global" objs >>= getNode "partials" >>= getLeaf' partPath of
    Just partFile -> case convertTP objs partFile of
                       Right x -> Right x
                       Left err -> Left $ "converting partial: " <> unlines err
    _ -> Left $ "partial file " <> show partPath <> " not found in global resource"
convertPartial _ s = Left ("error statement: "<> (show s))

convertForeach :: ObjectTree -> Stmt -> Either String Text
convertForeach objs@(ObjNode objs') (ForeachStmt holder dotObj stmts) =
  case convertDot2NodeList objs dotObj of
    Right nodeList -> go . mconcat $
      fmap (\node -> fmap (convertTP' (addRes node)) stmts) nodeList
    Left err -> Left $ "converting nodelist: " <> err
  where
    addRes node' = ObjNode (M.singleton holder (ObjNode node') <> objs')
    go x = if Prelude.null $ lefts x
              then Right $ mconcat . rights $ x
              else Left $ unlines $ lefts x
convertForeach _ s = Left ("error statement: "<> (show s))

convertDot2NodeList :: ObjectTree -> Stmt -> Either String [Map Text ObjectTree]
convertDot2NodeList objs (DotStmt mems) =
  let
    run (ObjNodeList s) [] = Right s
    run (ObjNode obj) (x:xs) = case obj !? x of
                                  Just a ->  run a xs
                                  _ -> Left (show x <> " on " <> show mems <> " not found in object tree:\n" <> showObjTree objs)
    run obj x = (Left ("cannot match the type ObjectNode with actual type" <> getType obj <> " in " <> show x)) in
  run objs mems
convertDot2NodeList _ s = Left ("error statement: "<> (show s))

convertDot :: ObjectTree -> Stmt -> Either String Text
convertDot objs (DotStmt mems) =
  let
    run (ObjLeaf s) [] = Right s
    run (ObjNode obj) (x:xs) = case obj !? x of
                                  Just a -> run a xs
                                  _      -> Left (show x <> " on " <> show mems <> " not found in " <> showObjTree objs)
    run obj x = (Left ("cannot match the type ObjectNode with actual type" <> getType obj <> " in " <> show x)) in
  run objs mems
convertDot _ s = Left ("error statement: "<> (show s))



getNode :: Text -> ObjectTree -> Maybe ObjectTree
getNode key (ObjNode objs) = case objs !? key of
                               Just x@(ObjNode _) -> Just x
                               _ -> Nothing
getNode _ _ = Nothing

getLeaf :: Text -> ObjectTree -> Maybe ObjectTree
getLeaf key (ObjNode objs) = case objs !? key of
                               Just x@(ObjLeaf _) -> Just x
                               _ -> Nothing
getLeaf _ _ = Nothing

getNode' :: Text -> ObjectTree -> Maybe (Map Text ObjectTree)
getNode' key obj = case getNode key obj of
                     Just (ObjNode x) -> Just x
                     _ -> Nothing

getLeaf' :: Text -> ObjectTree -> Maybe Text
getLeaf' key obj = case getLeaf key obj of
                     Just (ObjLeaf x) -> Just x
                     _ -> Nothing

singletonObjNode :: [Text] -> Map Text ObjectTree -> Map Text ObjectTree
singletonObjNode key x = (go key) x
  where go [] = id
        go (y:ys) = (singleton y) . ObjNode . (go ys)


addGlb :: Map Text ObjectTree -> ObjectTree-> ObjectTree
addGlb glbRes x = ObjNode (M.singleton "this" x <> glbRes)

toNodeList :: [ObjectTree] -> ObjectTree
toNodeList = ObjNodeList . fmap (\(ObjNode x) -> x)

getDate :: ObjectTree -> Maybe UTCTime
getDate obj = (\x -> parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" (T.unpack x) :: UTCTime) <$> (getLeaf' "date" obj)

getCategory :: ObjectTree -> Maybe Text
getCategory x = getLeaf' "category" x
