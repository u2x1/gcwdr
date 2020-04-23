{-# LANGUAGE OverloadedStrings #-}
module Markdown.Convert where

import Data.Attoparsec.ByteString
import Data.ByteString as BS (ByteString, init, last)
import Data.List (intersperse)
import Markdown.Type
import Markdown.Parser

convertMD :: ByteString -> ByteString
convertMD s = case parseOnly (many' mdElem) (s <> "\n") of
                Right [] -> ""
                Right xs -> concatAndInit $ convertMD' <$> xs
                _ -> ""

convertMD' :: MDElem -> ByteString
convertMD' (Header1 x) = addElemTag' "h1" x
convertMD' (Header2 x) = addElemTag' "h2" x
convertMD' (Header3 x) = addElemTag' "h3" x
convertMD' (Header4 x) = addElemTag' "h4" x
convertMD' (Header5 x) = addElemTag' "h5" x
convertMD' (Header6 x) = addElemTag' "h6" x
convertMD' (Header7 x) = addElemTag' "h7" x
convertMD' HorizontalRule         = "<hr>\n"
convertMD' (Paragrah xs)          = addElemTag' "p" $ concatAndInit $ convertMD' <$> xs
convertMD' (Blockquotes xs)       = addElemTag'' "blockquote" $ concatAndInit $ convertMD' <$> xs
convertMD' (OrderedList xs)       = addElemTag'' "ol" $ concatAndInit $ convertMD' <$> xs
convertMD' (UnorderedList xs)     = addElemTag'' "ul" $ concatAndInit $ convertMD' <$> xs
convertMD' (ListElem x)           = addElemTag "li" x
convertMD' (PlainText x)          = x
convertMD' (Italic x)             = addElemTag "em" x
convertMD' (Bold x)               = addElemTag "strong" x
convertMD' (BoldAndItalic x)      = addElemTag "strong" $ addElemTag "em" x
convertMD' (Strikethrough x)      = addElemTag "s" x
convertMD' (Link text url title)  = addElemTagwithProp "a" [("href", Just url), ("title", title)] text
convertMD' (Image text url title) = addElemTagwithProp' "img" [("src", Just url), ("alt", Just text), ("title", title)]
convertMD' (Code x)               = addElemTag "code" x
convertMD' (CodeBlock x)          = addElemTag'' "pre" $ addElemTag'' "code" x

concatAndInit :: [ByteString] -> ByteString
concatAndInit x
  | mconcat x == "" = mconcat x
  | BS.last (mconcat x) == 10 = BS.init $ mconcat x
  | otherwise = mconcat x

addElemTag :: ByteString -> ByteString -> ByteString
addElemTag tag x = mconcat ["<", tag, ">", x, "</", tag, ">"]

addElemTag' :: ByteString -> ByteString -> ByteString
addElemTag' tag x = mconcat ["<", tag, ">", x, "</", tag, ">\n"]

addElemTag'' :: ByteString -> ByteString -> ByteString
addElemTag'' tag x = mconcat ["<", tag, ">\n", x, "\n</", tag, ">\n"]

addElemTagwithProp :: ByteString -> [(ByteString, Maybe ByteString)] -> ByteString -> ByteString
addElemTagwithProp tag prop x = mconcat ["<", tag, " ", mconcat.intersperse " " $ makeProp <$> prop, ">", x, "</", tag, ">\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

addElemTagwithProp' :: ByteString -> [(ByteString, Maybe ByteString)] -> ByteString
addElemTagwithProp' tag prop = mconcat ["<", tag, " ", mconcat.intersperse " " $ makeProp <$> prop, "/>\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]
