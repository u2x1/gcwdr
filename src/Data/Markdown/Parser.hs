{-# LANGUAGE OverloadedStrings #-}
module Data.Markdown.Parser where

import           Control.Applicative            ( Alternative((<|>), many, some)
                                                )
import           Data.Attoparsec.Combinator     ( count
                                                , endOfInput
                                                , lookAhead
                                                , many'
                                                , manyTill
                                                , manyTill'
                                                )
import           Data.Attoparsec.Text          as APT
                                                ( Parser
                                                , anyChar
                                                , char
                                                , inClass
                                                , isEndOfLine
                                                , parseOnly
                                                , satisfy
                                                , string
                                                , takeTill
                                                , takeWhile
                                                , takeWhile1
                                                )
import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Data.Text                     as T
                                                ( Text
                                                , pack
                                                , singleton
                                                , unlines
                                                )

import           Data.Markdown.Type             ( MDElem
                                                  ( Blockquotes
                                                  , Bold
                                                  , BoldAndItalic
                                                  , Code
                                                  , CodeBlock
                                                  , Footnote
                                                  , FootnoteRef
                                                  , Header
                                                  , HorizontalRule
                                                  , Image
                                                  , Italic
                                                  , Link
                                                  , ListElem
                                                  , OrderedList
                                                  , Paragrah
                                                  , PlainText
                                                  , Strikethrough
                                                  , UnorderedList
                                                  , RawHtmlTag
                                                  )
                                                )
import           Prelude                 hiding ( takeWhile )

-- | Post staff




-- | Markdown staff

mdElem :: Parser MDElem
mdElem = many' eol >>
  rawHtmlTag
    <|> footnoteRef
    <|> blockquotes
    <|> orderedList
    <|> unorderedList
    <|> codeBlock
    <|> header
    <|> hrztRule
    <|> para

paraElem :: Parser MDElem
paraElem =
  escapeChar
    <|> italic
    <|> bold
    <|> boldAndItalic
    <|> strikethrough
    <|> footnote
    <|> link
    <|> image
    <|> code
    <|> plainText


escapeChar :: Parser MDElem
escapeChar = do
  _ <- char '\\'
  x <- T.singleton <$> satisfy (`elem` ("\\`*_{[(#+-.!|" :: String))
  return (PlainText x)

rawHtmlTag :: Parser MDElem
rawHtmlTag = do
    tagName <- char '<' *> takeTill (\x -> x == ' ' || x == '>') <* many (char ' ')
    props <- takeTill (== '>') <* char '>'
    let end = string "</" <* string tagName <* char '>'
    content <- T.pack <$> manyTill anyChar end <* many eol
    return (RawHtmlTag tagName props content)

para :: Parser MDElem
para = Paragrah <$> do
  text  <- takeTill isEndOfLine
  paras <- case parseOnly (some paraElem) text of
    Right x -> pure x
    Left  x -> fail x
  _ <- many' eol
  return paras

plainText :: Parser MDElem
plainText = PlainText <$> do
  w    <- anyChar
  text <- takeTill (inClass "![_*`\\\n")
  return (T.singleton w <> text)

emphasis :: Parser Text
emphasis = lookAhead (satisfy (not <$> isAstrOrUds)) *> takeTill isAstrOrUds

italic :: Parser MDElem
italic = do
  m    <- satisfy isAstrOrUds
  text <- emphasis
  _    <- satisfy (== m)
  return (Italic text)

bold :: Parser MDElem
bold = do
  m    <- count 2 $ satisfy isAstrOrUds
  text <- emphasis
  _    <- string $ pack . reverse $ m
  return (Bold text)

boldAndItalic :: Parser MDElem
boldAndItalic = do
  m    <- count 3 $ satisfy isAstrOrUds
  text <- emphasis
  _    <- string $ pack . reverse $ m
  return (BoldAndItalic text)

strikethrough :: Parser MDElem
strikethrough =
  Strikethrough . pack <$> (string "~~" *> manyTill' anyChar (string "~~"))

linkAndImageBracket :: Parser (Text, Text, Maybe Text)
linkAndImageBracket = do
  text <- char '[' *> takeTill (== ']') <* char ']'  -- [ *> text *< ]
  url  <- char '(' *> takeTill (\w -> w == ' ' || w == ')')  -- '(' *> url *< (' ' || ')')
  flag <- anyChar
  if flag == ' '
    then do
      title <- many' (char ' ') *> char '\"' *> takeTill (== '\"') <* string
        "\")"
      return (text, url, Just title)
    else pure (text, url, Nothing)

link :: Parser MDElem
link = do
  (text', url, title) <- linkAndImageBracket
  let text = case parseOnly (some paraElem) text' of
        Right x -> x
        Left  _ -> [PlainText text']
  return (Link text url title)

image :: Parser MDElem
image = do
  _                  <- char '!'
  (text, url, title) <- linkAndImageBracket
  return (Image text url title)

code :: Parser MDElem
code = do
  _    <- char '`'
  text <- takeWhile1 (/= '`')
  _    <- char '`'
  return (Code text)

codeBlock :: Parser MDElem
codeBlock = do
  _ <- string "```" <* many eol
  CodeBlock
    .   pack
    <$> (manyTill anyChar (many eol >> many (char ' ') >> string "```") <* eol)

hrztRule :: Parser MDElem
hrztRule = do
  _ <- count 3 (satisfy isAstrOrUdsOrDash) *> eol
  HorizontalRule <$ eol
  where isAstrOrUdsOrDash w = isAstrOrUds w || w == '-'

eol :: Parser Char
eol = satisfy isEndOfLine

blockquotes :: Parser MDElem
blockquotes = do
  cnt  <- length <$> lookAhead (some prefix <* some (char ' '))
  text <-
    T.unlines <$> some
      (count cnt prefix *> some (char ' ') *> takeTill isEndOfLine <* many' eol)

  case parseOnly (some mdElem) text of
    Right mdElems -> return (Blockquotes mdElems)
    Left  _       -> return (Blockquotes [PlainText text])
  where prefix = char '>'

orderedList :: Parser MDElem
orderedList = OrderedList . mconcat <$> some
  (some (satisfy isDigit) *> char '.' *> listElem)

unorderedList :: Parser MDElem
unorderedList = UnorderedList . mconcat <$> some
  (satisfy (\w -> w == '*' || w == '+' || w == '-') *> listElem)

listElem :: Parser [MDElem]
listElem = do
  _    <- some (char ' ')
  text <- takeTill isEndOfLine <* many' eol
  let lElem = case parseOnly (some paraElem) text of
        Right p -> p
        Left  _ -> [PlainText text]

  s <- ('x' <$ endOfInput) <|> lookAhead anyChar
  if s == ' '
    then do
      spaceCnt <- Prelude.length <$> lookAhead (some (char ' '))
      let indent = T.pack $ take spaceCnt [' ', ' ' ..]
      line <- some
        (   string indent
        *>  takeTill (== '\n')
        >>= (\x -> (x <>) <$> takeWhile (== '\n'))
        )

      let innerElems = case parseOnly (some mdElem) (T.unlines line) of
            Right p -> p
            Left  _ -> [PlainText text]
      return $ ListElem lElem : innerElems
    else return [ListElem lElem]

header :: Parser MDElem
header = do
  headerSize <- length <$> some (char '#')
  _          <- some (char ' ')
  text       <- takeTill isEndOfLine <* many eol
  return (Header headerSize text 0)

footnote :: Parser MDElem
footnote = Footnote <$> (string "[^" *> takeTill (== ']') <* char ']')

footnoteRef :: Parser MDElem
footnoteRef = do
  identity <- string "[^" *> takeTill (== ']') <* string "]:" <* many (char ' ')
  fstElem  <- takeTill isEndOfLine <* many eol
  fElem    <- case parseOnly mdElem fstElem of
    Right x -> pure [x]
    _       -> pure []
  iElems <- mconcat <$> many (elemInside <* many eol)
  return (FootnoteRef identity (addSign (fElem <> iElems) identity))

 where
  returnNote id' =
    [PlainText "  ", Link [PlainText "↩"] ("#fnref:" <> id') Nothing]

  addSign xs id' = init xs <> case Prelude.last xs of
    Paragrah x -> [Paragrah (x <> returnNote id')]
    x          -> x : returnNote id'

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
