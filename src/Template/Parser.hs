{-# LANGUAGE OverloadedStrings #-}
module Template.Parser where

import Data.Attoparsec.ByteString
import Template.Type
import Control.Applicative
import Data.ByteString (pack, singleton)

template :: Parser [Stmt]
template = many (stmt <|> raw)

stmt :: Parser Stmt
stmt = do
  string "[-" *> many (word8 32)
  s <- pack <$> manyTill anyWord8 (string "-]")
  case parseOnly dotStmt s of
    Left _ -> fail "Parse statement failed."
    Right x -> return x

dotStmt :: Parser Stmt
dotStmt = do
  obj <- takeTill (== 46)
  member <- some (word8 46 *> takeTill (\w -> w == 46 || w == 32))
  return (DotStmt obj member)

raw :: Parser Stmt
raw = do
  w <- anyWord8
  Raw . (singleton w <>) <$> takeTill (==91)
