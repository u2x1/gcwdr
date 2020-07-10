{-# LANGUAGE OverloadedStrings #-}
module Data.Markdown.Parser where

import           Data.Attoparsec.Text       as APT
import           Data.Attoparsec.Combinator
import           Data.Text                  as T  (singleton, Text, pack, last)
import           Data.Char
import           Control.Applicative

import           Data.Markdown.Type

mdElem :: Parser MDElem
mdElem = footnoteRef <|> blockquotes <|> orderedList <|> unorderedList <|> codeBlock <|> header <|> hrztRule <|> para

escapeChar :: Parser MDElem
escapeChar = do
  _ <- char '\\'
  x <- T.singleton <$> satisfy (`elem` ("\\`*_{[(#+-.!|" :: String))
  return (PlainText x)

para :: Parser MDElem
para = Paragrah <$> do
  text <- takeTill isEndOfLine
  paras <- case parseOnly (some paraElem) text of
             Right x -> pure x
             Left x -> fail x
  _ <- many' (satisfy isEndOfLine)
  return paras

paraElem :: Parser MDElem
paraElem = escapeChar <|> italic <|> bold <|> boldAndItalic <|> strikethrough <|> footnote <|> link <|> image <|> code <|> plainText

plainText :: Parser MDElem
plainText = PlainText <$> do
  w <- anyChar
  text <- takeTill (inClass "![_*`\\\n")
  return (T.singleton w <> text)

emphasis :: Parser Text
emphasis =
  lookAhead (satisfy (not <$> isAstrOrUds)) *>
  takeTill isAstrOrUds

italic :: Parser MDElem
italic = do
  m <- satisfy isAstrOrUds
  text <- emphasis
  _ <- satisfy (== m)
  return (Italic text)

bold :: Parser MDElem
bold = do
  m <- count 2 $ satisfy isAstrOrUds
  text <- emphasis
  _ <- string $ pack.reverse $ m
  return (Bold text)

boldAndItalic :: Parser MDElem
boldAndItalic = do
  m <- count 3 $ satisfy isAstrOrUds
  text <- emphasis
  _ <- string $ pack.reverse $ m
  return (BoldAndItalic text)

strikethrough :: Parser MDElem
strikethrough = Strikethrough . pack <$> (string "~~" *> manyTill' anyChar (string "~~"))

linkAndImageBracket :: Parser (Text, Text, Maybe Text)
linkAndImageBracket = do
  text <- char '[' *> takeTill (== ']') <* char ']'  -- [ *> text *< ]
  url <- char '(' *> takeTill (\w -> w == ' ' || w == ')')  -- '(' *> url *< (' ' || ')')
  flag <- anyChar
  if flag == ' '
     then do
       title <- many' (char ' ') *> char '\"' *> takeTill (== '\"') <* string "\")"
       return (text, url, Just title)
     else pure (text, url, Nothing)

link :: Parser MDElem
link = do
  (text', url, title) <- linkAndImageBracket
  let text = case parseOnly (some paraElem) text' of
               Right x -> x
               Left _ -> [PlainText text']
  return (Link text url title)

image :: Parser MDElem
image = do
  _ <- char '!'
  (text, url, title) <- linkAndImageBracket
  return (Image text url title)

code :: Parser MDElem
code = do
  _ <- char '`'
  text <- takeWhile1 (/= '`')
  _ <- char '`'
  return (Code text)

codeBlock :: Parser MDElem
codeBlock = do
  _ <- string "```" <* skipEndOfLine
  CodeBlock . pack <$> (manyTill anyChar (string "\n```") <* skipEndOfLine)

hrztRule :: Parser MDElem
hrztRule = do
  _ <- count 3 (satisfy isAstrOrUdsOrDash) *> satisfy isEndOfLine
  HorizontalRule <$ skipEndOfLine
  where
    isAstrOrUdsOrDash w = isAstrOrUds w || w == '-'

skipEndOfLine :: Parser [Char]
skipEndOfLine = many (satisfy isEndOfLine)

blockquotes :: Parser MDElem
blockquotes = do
  cnt <- length <$> lookAhead (some takePrefix)
  text <- mconcat <$> some ((<>"\n") <$> (count cnt takePrefix *> takeTill isEndOfLine <* many' (satisfy isEndOfLine)))
  case parseOnly (some mdElem) text of
    Right mdElems -> return (Blockquotes mdElems)
    Left _ -> return (Blockquotes [PlainText text])
  where
    takePrefix = char '>' <* many (char ' ')

orderedList :: Parser MDElem
orderedList = OrderedList . mconcat <$> some (some (satisfy isDigit) *> char '.' *> listElem 0)

unorderedList :: Parser MDElem
unorderedList = UnorderedList . mconcat <$> some (satisfy isAstrOrDash *> listElem 0)

orderedList' :: Int -> Parser MDElem
orderedList' indent = OrderedList . mconcat <$> some (count indent (char ' ') *> some (satisfy isDigit) *> char '.' *> listElem indent)

unorderedList' :: Int ->  Parser MDElem
unorderedList' indent = UnorderedList . mconcat <$> (some (count indent (char ' ') *> satisfy isAstrOrDash *> listElem indent) <* skipEndOfLine)

listElem :: Int -> Parser [MDElem]
listElem hIndent = do
  _ <- some (char ' ')
  text <- takeTill isEndOfLine <* many' (satisfy isEndOfLine)
  let lElem = case parseOnly (some paraElem) text of
                Right p -> p
                Left _ -> [PlainText text]
  if T.last text /= '\n'
     then return [ListElem lElem]
     else do
       s <- pack <$> lookAhead (count (hIndent + 1) anyChar)
       if s == mconcat (replicate (hIndent + 1) " ")
          then do
            spaceCnt <- lookAhead (count hIndent (char ' ') *> some (char ' '))
            let indent = hIndent + Prelude.length spaceCnt
            inListElem <-
              codeBlock <|> blockquotes <|> image <|> orderedList' indent <|> unorderedList' indent <|> para
            return [ListElem lElem, inListElem]
          else return [ListElem lElem]

header :: Parser MDElem
header = do
  headerSize <- length <$> some (char '#')
  _ <- some (char ' ')
  text <- takeTill isEndOfLine <* skipEndOfLine
  return (Header headerSize text)

footnote :: Parser MDElem
footnote = Footnote <$> (string "[^" *> takeTill (== ']') <* char ']')

footnoteRef :: Parser MDElem
footnoteRef = do
  identity <- string "[^" *> takeTill (== ']') <* string "]:" <* many (char ' ')
  fstElem <- takeTill isEndOfLine <* many (satisfy isEndOfLine)
  fElem <- case parseOnly mdElem fstElem of
             Right x -> pure [x]
             _ -> pure []
  iElems <- mconcat <$> many (elemInside <* many (satisfy isEndOfLine))
  return (FootnoteRef identity (addSign (fElem <> iElems) identity))
  where
    addSign xs identity = init xs <>
      case Prelude.last xs of
        Paragrah x -> [Paragrah (x <> [Link [PlainText ("↩")] ("#fnref:" <> identity) Nothing])]
        x -> x : [Link [PlainText ("↩")] ("#fnref:" <> identity) Nothing]
    elemInside = do
      s <- lookAhead anyChar
      if s == ' '
         then do
           _ <- many' (satisfy isSpace)
           pure <$> mdElem
         else fail ""

-- * or _
isAstrOrUds :: Char -> Bool
isAstrOrUds w = w == '_' || w == '*'

-- * or -
isAstrOrDash :: Char -> Bool
isAstrOrDash w = w == '*' || w == '+' || w == '-'