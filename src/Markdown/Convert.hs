{-# LANGUAGE OverloadedStrings #-}
module Markdown.Convert where

import Data.Word8
import Data.Attoparsec.ByteString
import Data.ByteString as BS (ByteString, init, last, pack, unpack)
import Data.ByteString.UTF8 (fromString)
import Data.List (intersperse)
import Markdown.Type
import Markdown.Parser

convertMD :: ByteString -> ByteString
convertMD s = case parseOnly (many' mdElem) (s <> "\n") of
                Right [] -> ""
                Right xs -> concatAndInit $ convertMD' <$> xs
                _ -> ""

convertMD' :: MDElem -> ByteString
convertMD' (Header hz x) = addElemTag' ("h" <> fromString (show hz)) x
convertMD' HorizontalRule         = "<hr>\n"
convertMD' (Paragrah xs)          = addElemTag' "p" $ concatAndInit $ convertMD' <$> xs
convertMD' (Blockquotes xs)       = addElemTag'' "blockquote" $ concatAndInit $ convertMD' <$> xs
convertMD' (OrderedList xs)       = addElemTag'' "ol" $ concatAndInit $ convertMD' <$> xs
convertMD' (UnorderedList xs)     = addElemTag'' "ul" $ concatAndInit $ convertMD' <$> xs
convertMD' (ListElem xs)          = addElemTag "li" $ concatAndInit $ convertMD' <$> xs
convertMD' (PlainText x)          = x
convertMD' (Italic x)             = addElemTag "em" x
convertMD' (Bold x)               = addElemTag "strong" x
convertMD' (BoldAndItalic x)      = addElemTag "strong" $ addElemTag "em" x
convertMD' (Strikethrough x)      = addElemTag "s" x
convertMD' (Link text url title)  = addElemTagwithProp "a" [("href", Just url), ("title", title)] (concatAndInit $ convertMD' <$> text)
convertMD' (Image text url title) = addElemTagwithProp' "img" [("src", Just url), ("alt", Just text), ("title", title)]
convertMD' (Code x)               = addElemTag "code" $ escapeHTML x
convertMD' (CodeBlock x)          = addElemTag' "pre" $ addElemTag "code" $ escapeHTML x

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

escapeHTML :: ByteString -> ByteString
escapeHTML x = pack $ mconcat $ escapeWord8 <$> unpack x

escapeWord8 :: Word8 -> [Word8]
escapeWord8 60 = unpack "&lt;"
escapeWord8 62 = unpack "&gt;"
escapeWord8 38 = unpack "&amp;"
escapeWord8 34 = unpack "&quot;"
escapeWord8 39 = unpack "&#39;"
escapeWord8 x = [x]
