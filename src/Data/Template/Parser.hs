{-# LANGUAGE OverloadedStrings #-}
module Data.Template.Parser where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Word8
import Data.Text            as T (pack, singleton)

import Data.Template.Type

stmt :: Parser Stmt
stmt = foreachStmt <|> stmt' <|> raw

stmt' :: Parser Stmt
stmt' = do
  _ <- string "[-" *> many (char ' ')
  s <- T.pack <$> manyTill anyChar (many (char ' ') *> string "-]")
  case parseOnly (partialsStmt <|> dotStmt) s of
    Left _ -> fail "Parse statement failed."
    Right x -> return x

dotStmt :: Parser Stmt
dotStmt = do
  obj <- takeTill (\w -> w == '.' || w == ' ')
  mem <- some (char '.' *> takeTill (\w -> w == '.' || w == ' '))
  return (DotStmt (obj:mem))

partialsStmt :: Parser Stmt
partialsStmt = do
  _ <- string "partial" *> some (char ' ')
  pFile <- takeTill (== ' ')
  return (PartialStmt pFile)

foreachStmt :: Parser Stmt
foreachStmt = do
  _ <- string "[- foreach" *> many (char ' ')
  placeholder <- takeTill (== ' ')
  _ <- many (char ' ') *> string "in" *> many (char ' ')
  obj <- dotStmt <* many (char ' ') <* string "-]" <* many (satisfy isEndOfLine)
  inSpaceStmt <- manyTill stmt (string "[- end -]")
  return (ForeachStmt placeholder obj inSpaceStmt)

raw :: Parser Stmt
raw = do
  w <- anyChar -- always ignore first character
  Raw . (T.singleton w <>) <$> takeTill (== '[') -- take till meeting '['

-- * or _
isAstrOrUds :: Word8 -> Bool
isAstrOrUds w = w == 95 || w == 42

-- * or -
isAstrOrDash :: Word8 -> Bool
isAstrOrDash w = w == 42 || w == 43 || w == 45
