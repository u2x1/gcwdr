{-# LANGUAGE OverloadedStrings #-}
module Template.Convert where

import Template.Type
import Data.Map.Lazy
import Data.ByteString as BS
import System.IO (FilePath)
import Data.Attoparsec.ByteString
import Template.Parser
import Data.Maybe

parsePost :: FilePath -> IO (Maybe ObjectTree)
parsePost path = do
  s <- BS.readFile path
  case parseOnly post s of
    Right x -> pure (Just x)
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
convertTP' (ObjLeaf _) _ = Nothing
convertTP' _ (Raw rawHtml) = Just rawHtml
convertTP' _ IfStmt = Nothing
convertTP' (ObjNode objs) (DotStmt obj mems) =
  let
    run _ (ObjLeaf s) [] = Just s
    run _ (ObjLeaf _) _ = Nothing
    run objs' (ObjNode obj') (x:xs) = run objs' (obj' ! x) xs in
  if obj /= ""
     then run objs (objs ! obj) mems
     else run objs (ObjNode objs) mems
