{-# LANGUAGE OverloadedStrings #-}
module Data.Template.Parser where

import           Control.Applicative            ( Alternative((<|>), many, some)
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , anyChar
                                                , char
                                                , isEndOfLine
                                                , manyTill
                                                , parseOnly
                                                , satisfy
                                                , string
                                                , takeTill
                                                )
import           Data.Text                     as T
                                                ( pack
                                                , singleton
                                                )
import           Data.Word8                     ( Word8 )

import           Data.Template.Type             ( Stmt(..) )

stmt :: Parser Stmt
stmt = ifdefStmt <|> foreachStmt <|> stmt' <|> raw

stmt' :: Parser Stmt
stmt' = do
  _ <- string "[-" *> many (char ' ')
  s <- T.pack <$> manyTill anyChar (many (char ' ') *> string "-]")
  case parseOnly (partialsStmt <|> dotStmt) s of
    Left  _ -> fail "parse statement failed."
    Right x -> return x

dotStmt :: Parser Stmt
dotStmt = do
  obj <- takeTill (\w -> w == '.' || w == ' ')
  mem <- some (char '.' *> takeTill (\w -> w == '.' || w == ' '))
  return (DotStmt (obj : mem))

partialsStmt :: Parser Stmt
partialsStmt = do
  _     <- string "partial" *> some (char ' ')
  pFile <- takeTill (== ' ')
  return (PartialStmt pFile)

ifdefStmt :: Parser Stmt
ifdefStmt = do
  _          <- string "[- ifdef" *> many (char ' ')
  obj <- dotStmt <* many (char ' ') <* string "-]" <* many (satisfy isEndOfLine)
  trueStmts  <- manyTill stmt (string "[- else -]")
  falseStmts <- manyTill stmt (string "[- end -]")
  return (IfdefStmt obj trueStmts falseStmts)



foreachStmt :: Parser Stmt
foreachStmt = do
  _           <- string "[- foreach" *> many (char ' ')
  placeholder <- takeTill (== ' ')
  _           <- many (char ' ') *> string "in" *> many (char ' ')
  obj <- dotStmt <* many (char ' ') <* string "-]" <* many (satisfy isEndOfLine)
  inSpaceStmt <- manyTill stmt (many (char ' ') *> string "[- end -]")
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
