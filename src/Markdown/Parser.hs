{-# LANGUAGE OverloadedStrings #-}
module Markdown.Parser where

import Data.Attoparsec.ByteString as APB
import Data.Attoparsec.Combinator
import Data.ByteString as BS (ByteString, pack, singleton)
import Data.Functor
import Data.Word8
import Control.Applicative
import Data.Map.Lazy as M
import Markdown.Type
import Template.Type

metaData :: Parser (Map ByteString ObjectTree)
metaData = do
  _ <- many (string "---\n")
  els <- manyTill el (string "---")
  _ <- many (word8 10)
  return (ObjLeaf <$> fromList els)
  where el = do
          obj <- takeTill (== 58) <* word8 58 <* many (word8 32)
          text <- takeTill isEndOfLine <* satisfy isEndOfLine
          return (obj, text)

mdElem :: Parser MDElem
mdElem = blockquotes <|> orderedList <|> unorderedList <|> codeBlock <|> header <|> hrztRule <|> para

para :: Parser MDElem
para = Paragrah <$> do
  paras <- manyTill (italic <|> bold <|> boldAndItalic <|> strikethrough <|> link <|> image <|> code <|> plainText)
              (satisfy isEndOfLine)
  _ <- many' (satisfy isEndOfLine)
  return paras

plainText :: Parser MDElem
plainText = PlainText <$> do
  w <- anyWord8
  text <- takeTill (inClass "![_*`\n")
  return (BS.singleton w <> text)

emphasis :: Parser ByteString
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
strikethrough = Strikethrough . pack <$> (string "~~" *> manyTill' anyWord8 (string "~~"))

linkAndImageBracket :: Parser (ByteString, ByteString, Maybe ByteString)
linkAndImageBracket = do
  text <- word8 91 *> takeTill (== 93) <* word8 93
  url <- word8 40 *> takeTill (\w -> w == 32 || w == 41)
  s <- lookAhead (many' (word8 32) *> anyWord8)
  if s == 34
     then do
       title <- many' (word8 32) *> word8 34 *> takeTill (== 34) <* word8 34 <* word8 41
       return (text, url, Just title)
       else word8 41 $> (text, url, Nothing)

link :: Parser MDElem
link = do
  (text, url, title) <- linkAndImageBracket
  return (Link text url title)

image :: Parser MDElem
image = do
  _ <- word8 33
  (text, url, title) <- linkAndImageBracket
  return (Image text url title)

code :: Parser MDElem
code = do
  _ <- word8 96
  text <- takeWhile1 (/= 96)
  _ <- word8 96
  return (Code text)

codeBlock :: Parser MDElem
codeBlock = do
  _ <- string "```" <* skipEndOfLine
  CodeBlock . pack <$> (manyTill anyWord8 (string "```") <* skipEndOfLine)

hrztRule :: Parser MDElem
hrztRule = do
  _ <- count 3 (satisfy isAstrOrUdsOrDash) *> many' (satisfy isAstrOrUdsOrDash)
  HorizontalRule <$ skipEndOfLine
  where
    isAstrOrUdsOrDash w = isAstrOrUds w || w == 45

skipEndOfLine :: Parser [Word8]
skipEndOfLine = many (satisfy isEndOfLine)

blockquotes :: Parser MDElem
blockquotes = do
  cnt <- length <$> lookAhead (some takePrefix)
  text <- mconcat <$> some ((<>"\n") <$> (count cnt takePrefix *> takeTill isEndOfLine <* satisfy isEndOfLine))
  case parseOnly (some mdElem) text of
    Right mdElems -> return (Blockquotes mdElems)
    Left _ -> return (Blockquotes [PlainText text])
  where
    takePrefix = word8 62 <* many (word8 32)

orderedList :: Parser MDElem
orderedList = OrderedList . mconcat <$> some (some (satisfy isDigit) *> word8 46 *> listElem 0)

unorderedList :: Parser MDElem
unorderedList = UnorderedList . mconcat <$> some (satisfy isAstrOrDash *> listElem 0)
  where isAstrOrDash w = w == 42 || w == 43 || w == 45

orderedList' :: Int -> Parser MDElem
orderedList' indent = OrderedList . mconcat <$> some (count indent (word8 32) *> some (satisfy isDigit) *> word8 46 *> listElem indent)

unorderedList' :: Int ->  Parser MDElem
unorderedList' indent = UnorderedList . mconcat <$> (some (count indent (word8 32) *> satisfy isAstrOrDash *> listElem indent) <* skipEndOfLine)
  where isAstrOrDash w = w == 42 || w == 43 || w == 45

listElem :: Int -> Parser [MDElem]
listElem hIndent = do
  _ <- some (word8 32)
  text <- takeTill isEndOfLine <* satisfy isEndOfLine
  s <- pack <$> lookAhead (count (hIndent+1) anyWord8)
  if s == mconcat (replicate (hIndent + 1) " ")
     then do
       spaceCnt <- many' (satisfy isEndOfLine) *> lookAhead (count hIndent (word8 32) *> some (word8 32))
       let indent = hIndent + Prelude.length spaceCnt
       inListElem <-
         codeBlock <|> image <|> orderedList' indent <|> unorderedList' indent <|> para <|> blockquotes
       return [ListElem text, inListElem]
     else return [ListElem text]


header :: Parser MDElem
header = h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6 <|> h7

h1 :: Parser MDElem
h1 = Header1 <$> header' 1

h2 :: Parser MDElem
h2 = Header2 <$> header' 2

h3 :: Parser MDElem
h3 = Header3 <$> header' 3

h4 :: Parser MDElem
h4 = Header4 <$> header' 4

h5 :: Parser MDElem
h5 = Header5 <$> header' 5

h6 :: Parser MDElem
h6 = Header6 <$> header' 6

h7 :: Parser MDElem
h7 = Header7 <$> header' 7

header' :: Int -> Parser ByteString
header' i = do
  _ <- count i (word8 35)
  _ <- lookAhead (notWord8 35)
  _ <- some (word8 32)
  takeTill isEndOfLine <* skipEndOfLine

isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 10 || w == 13

isAstrOrUds :: Word8 -> Bool
isAstrOrUds w = w == 95 || w == 42
