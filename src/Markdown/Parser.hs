{-# LANGUAGE OverloadedStrings #-}
module Markdown.Parser where

import Data.Attoparsec.ByteString as APB
import Data.Attoparsec.Combinator
import Data.ByteString.UTF8 (fromString)
import Data.ByteString as BS (ByteString, pack, singleton)
import qualified Data.ByteString as BS (last)
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
mdElem = footnoteRef <|> blockquotes <|> orderedList <|> unorderedList <|> codeBlock <|> header <|> hrztRule <|> para

escapeChar :: Parser MDElem
escapeChar = do
  _ <- word8 92
  x <- BS.singleton <$> satisfy (`elem` [92,96,42,95,123,91,40,35,43,45,46,33,124])
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
  w <- anyWord8
  text <- takeTill (inClass "![_*`\\\n")
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
  text <- word8 91 *> takeTill (== 93) <* word8 93  -- [ *> text *< ]
  url <- word8 40 *> takeTill (\w -> w == 32 || w == 41)  -- '(' *> url *< (' ' || ')')
  flag <- anyWord8
  if flag == 32
     then do
       title <- many' (word8 32) *> word8 34 *> takeTill (== 34) <* string "\")"
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
  CodeBlock . pack <$> (manyTill anyWord8 (string "\n```") <* skipEndOfLine)

hrztRule :: Parser MDElem
hrztRule = do
  _ <- count 3 (satisfy isAstrOrUdsOrDash) *> satisfy isEndOfLine
  HorizontalRule <$ skipEndOfLine
  where
    isAstrOrUdsOrDash w = isAstrOrUds w || w == 45

skipEndOfLine :: Parser [Word8]
skipEndOfLine = many (satisfy isEndOfLine)

blockquotes :: Parser MDElem
blockquotes = do
  cnt <- length <$> lookAhead (some takePrefix)
  text <- mconcat <$> some ((<>"\n") <$> (count cnt takePrefix *> takeTill isEndOfLine <* many' (satisfy isEndOfLine)))
  case parseOnly (some mdElem) text of
    Right mdElems -> return (Blockquotes mdElems)
    Left _ -> return (Blockquotes [PlainText text])
  where
    takePrefix = word8 62 <* many (word8 32)

orderedList :: Parser MDElem
orderedList = OrderedList . mconcat <$> some (some (satisfy isDigit) *> word8 46 *> listElem 0)

unorderedList :: Parser MDElem
unorderedList = UnorderedList . mconcat <$> some (satisfy isAstrOrDash *> listElem 0)

orderedList' :: Int -> Parser MDElem
orderedList' indent = OrderedList . mconcat <$> some (count indent (word8 32) *> some (satisfy isDigit) *> word8 46 *> listElem indent)

unorderedList' :: Int ->  Parser MDElem
unorderedList' indent = UnorderedList . mconcat <$> (some (count indent (word8 32) *> satisfy isAstrOrDash *> listElem indent) <* skipEndOfLine)

listElem :: Int -> Parser [MDElem]
listElem hIndent = do
  _ <- some (word8 32)
  text <- takeTill isEndOfLine <* many' (satisfy isEndOfLine)
  let lElem = case parseOnly (some paraElem) text of
                Right p -> p
                Left _ -> [PlainText text]
  if BS.last text /= 10
     then return [ListElem lElem]
     else do
       s <- pack <$> lookAhead (count (hIndent + 1) anyWord8)
       if s == mconcat (replicate (hIndent + 1) " ")
          then do
            spaceCnt <- lookAhead (count hIndent (word8 32) *> some (word8 32))
            let indent = hIndent + Prelude.length spaceCnt
            inListElem <-
              codeBlock <|> blockquotes <|> image <|> orderedList' indent <|> unorderedList' indent <|> para
            return [ListElem lElem, inListElem]
          else return [ListElem lElem]

header :: Parser MDElem
header = do
  headerSize <- length <$> some (word8 35)
  _ <- some (word8 32)
  text <- takeTill isEndOfLine <* skipEndOfLine
  return (Header headerSize text)

footnote :: Parser MDElem
footnote = Footnote <$> (string "[^" *> takeTill (== 93) <* word8 93)

footnoteRef :: Parser MDElem
footnoteRef = do
  identity <- string "[^" *> takeTill (== 93) <* string "]:" <* many (word8 32)
  fstElem <- takeTill isEndOfLine <* many (satisfy isEndOfLine)
  fElem <- case parseOnly mdElem fstElem of
             Right x -> pure [x]
             _ -> pure []
  iElems <- mconcat <$> many (elemInside <* many (satisfy isEndOfLine))
  return (FootnoteRef identity (addSign (fElem <> iElems) identity))
  where
    addSign xs identity = init xs <>
      case last xs of
        Paragrah x -> [Paragrah (x <> [Link [PlainText (fromString "↩")] ("#fnref:" <> identity) Nothing])]
        x -> x : [Link [PlainText (fromString "↩")] ("#fnref:" <> identity) Nothing]
    elemInside = do
      s <- lookAhead anyWord8
      if s == 32
         then do
           many' (satisfy isSpace)
           pure <$> mdElem
         else fail ""

isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 10 || w == 13

isAstrOrUds :: Word8 -> Bool
isAstrOrUds w = w == 95 || w == 42

isAstrOrDash :: Word8 -> Bool
isAstrOrDash w = w == 42 || w == 43 || w == 45
