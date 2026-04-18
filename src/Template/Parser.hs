module Template.Parser where

import           Control.Applicative            ( Alternative((<|>), many, some)
                                                )
import           Data.Attoparsec.Combinator     ( lookAhead )
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
                                                , empty
                                                )

import           Template.Type                  ( Stmt(..) )

stmt :: Parser Stmt
stmt = do
  isIfdef <- (True <$ lookAhead (ctlPrefix "ifdef")) <|> pure False
  if isIfdef
    then ifdefStmt
    else do
      isIf <- (True <$ lookAhead (ctlPrefix "if")) <|> pure False
      if isIf
        then ifeqStmt
        else do
          isForeach <- (True <$ lookAhead (ctlPrefix "foreach")) <|> pure False
          if isForeach then foreachStmt else stmt' <|> raw
 where
  ctlPrefix kw = string "[-" *> many (char ' ') *> string kw

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
  if mem == [T.empty] then fail "no member" else pure ()
  return (DotStmt (obj : mem))

partialsStmt :: Parser Stmt
partialsStmt = do
  _     <- string "partial" *> some (char ' ')
  pFile <- takeTill (== ' ')
  return (PartialStmt pFile)

ifeqStmt :: Parser Stmt
ifeqStmt = do
  _    <- string "[-" *> many (char ' ') *> string "if" *> some (char ' ')
  obj  <- dotStmt <* many (char ' ')
  op   <- (string "==" <|> string "!=")
  _    <- many (char ' ')
  val  <- char '"' *> takeTill (== '"') <* char '"'
  _    <- many (char ' ') <* string "-]" <* many (satisfy isEndOfLine)
  (aStmts, flag) <- manyTill'' stmt end
  let negated = op == "!="
  if flag == "end"
    then return (IfEqStmt val obj aStmts [] negated)
    else do
      falseStmts <- manyTill stmt (tag "end")
      return (IfEqStmt val obj aStmts falseStmts negated)
 where
  tag kw =
    string "[-" *> many (char ' ') *> string kw <* many (char ' ') <* string "-]"
  end = tag "end" <|> tag "else"
  manyTill'' :: Parser a -> Parser b -> Parser ([a], b)
  manyTill'' p e = let scan = (e >>= (\x -> pure ([], x)))
                          <|> (liftA2 (\x (a, b) -> (x:a, b)) p scan) in
      scan

ifdefStmt :: Parser Stmt
ifdefStmt = do
  _    <- string "[-" *> many (char ' ') *> string "ifdef" *> some (char ' ')
  obj <- dotStmt <* many (char ' ') <* string "-]" <* many (satisfy isEndOfLine)
  (aStmts, flag)  <- manyTill'' stmt end
  if flag == "end"
    then return (IfdefStmt obj aStmts [])
    else do
      falseStmts <- manyTill stmt (tag "end")
      return (IfdefStmt obj aStmts falseStmts)
  where
    tag kw =
      string "[-" *> many (char ' ') *> string kw <* many (char ' ') <* string "-]"
    end = tag "end" <|> tag "else"
    manyTill'' :: Parser a -> Parser b -> Parser ([a], b)
    manyTill'' p e = let scan = (e >>= (\x -> pure ([], x)))
                            <|> (liftA2 (\x (a, b) -> (x:a, b)) p scan) in
        scan



foreachStmt :: Parser Stmt
foreachStmt = do
  _           <- string "[-" *> many (char ' ') *> string "foreach" *> some (char ' ')
  placeholder <- takeTill (== ' ')
  _           <- many (char ' ') *> string "in" *> many (char ' ')
  obj <- dotStmt <* many (char ' ') <* string "-]" <* many (satisfy isEndOfLine)
  inSpaceStmt <- manyTill stmt (tag "end")
  return (ForeachStmt placeholder obj inSpaceStmt)
 where
  tag kw =
    string "[-" *> many (char ' ') *> string kw <* many (char ' ') <* string "-]"

raw :: Parser Stmt
raw = do
  w <- anyChar -- always ignore first character
  Raw . (T.singleton w <>) <$> takeTill (== '[') -- take till meeting '['
