{-# LANGUAGE OverloadedStrings #-}
module Template.Parser where

import Data.Attoparsec.ByteString
import Template.Type
import Markdown.Type
import Markdown.Parser
import Markdown.Convert
import Control.Applicative
import Data.ByteString as BS (pack, singleton)
import Data.Map.Lazy as M
import Data.Maybe

post :: Parser ObjectTree
post = do
  meta' <- metaData
  let meta = if isJust (meta' !? "template") then meta' else meta'
                            <> M.singleton "template" (ObjLeaf "post")
  postContent <- takeByteString
  return $ ObjNode (meta <> M.singleton "content" (ObjLeaf $ convertMD postContent))

stmt :: Parser Stmt
stmt = stmt' <|> raw

stmt' :: Parser Stmt
stmt' = do
  _ <- string "[-" *> many (word8 32)
  s <- pack <$> manyTill anyWord8 (string "-]")
  case parseOnly (partialsStmt <|> dotStmt <|> foreachStmt) s of
    Left _ -> fail "Parse statement failed."
    Right x -> return x

dotStmt :: Parser Stmt
dotStmt = do
  obj <- takeTill (== 46)
  member <- some (word8 46 *> takeTill (\w -> w == 46 || w == 32))
  return (DotStmt obj member)

partialsStmt :: Parser Stmt
partialsStmt = do
  _ <- string "partial" *> many (word8 32)
  pFile <- takeTill (== 32)
  return (PartialStmt pFile)

foreachStmt :: Parser Stmt
foreachStmt = do
  _ <- string "foreach" *> many (word8 32)
  placeholder <- takeTill (==32)
  _ <- many (word8 32) *> string "in" *> many (word8 32)
  obj <- dotStmt <* many (word8 32)
  inSpaceStmt <- manyTill stmt (string "[- end -]")
  return (ForeachStmt placeholder obj inSpaceStmt)

raw :: Parser Stmt
raw = do
  w <- anyWord8
  Raw . (BS.singleton w <>) <$> takeTill (==91)
