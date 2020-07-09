{-# LANGUAGE OverloadedStrings #-}
module Data.Markdown where

import Control.Applicative (many)
import Data.Attoparsec.ByteString
import Data.List.Extra            as LE (init, dropWhileEnd)
import Data.ByteString.Search           (breakAfter)
import Data.ByteString            as BS (ByteString, init, last, pack, unpack, readFile)
import Data.ByteString.UTF8             (fromString)
import Data.List                        (intersperse)
import Data.Either                      (rights, lefts)
import Data.Map.Lazy              as M  (singleton, (!?), Map, fromList)
import Data.Maybe

import Data.Markdown.Type
import Data.Markdown.Parser
import Data.Template.Type

parsePost :: FilePath -> IO (Maybe ObjectTree)
parsePost path = do
  s <- BS.readFile path
  case toObjectTree <$> parseOnly post s of
    Right (ObjNode x) -> do
      let relPath = snd $ breakAfter "content/" $ fromString $ LE.init $ dropWhileEnd (/='.') path
      pure $ Just (ObjNode (M.singleton "relLink" (ObjLeaf (relPath <> "/")) <> x))
    _ -> pure Nothing


type MetaData = Map ByteString ObjectTree
data Post = Post MetaData ByteString

instance ToObjectTree Post where
  toObjectTree (Post meta content) = ObjNode (meta <> M.singleton "content" (ObjLeaf $ convertMD content))

post :: Parser Post
post = do
  meta' <- metaData
  let meta = if isJust (meta' !? "template")
                then meta'
                else meta' <> M.singleton "template" (ObjLeaf "post")  -- default template to "post"
  postContent <- takeByteString
  return $ Post meta (convertMD postContent)

metaData :: Parser MetaData
metaData = do
  _ <- many (string "---\n")
  els <- manyTill el (string "---")
  _ <- many (word8 10)
  return (ObjLeaf <$> fromList els)
  where el = do
          obj <- takeTill (== 58) <* word8 58 <* many (word8 32)
          text <- takeTill isEndOfLine <* satisfy isEndOfLine
          return (obj, text)

convertMD :: ByteString -> ByteString
convertMD s = case parseOnly (many' mdElem) (s <> "\n\n") of
                Right xs -> concat' (takeFn xs)
                _ -> ""

convertMD' :: MDElem -> ByteString
convertMD' (Header hz x) = tag' ("h" <> fromString (show hz)) x
convertMD' HorizontalRule         = "<hr>\n"
convertMD' (Paragrah xs)          = tag' "p" $ concat' xs
convertMD' (Blockquotes xs)       = tag'' "blockquote" $ concat' xs
convertMD' (OrderedList xs)       = tag'' "ol" $ concat' xs
convertMD' (UnorderedList xs)     = tag'' "ul" $ concat' xs
convertMD' (ListElem xs)          = tag "li" $ concat' xs
convertMD' (PlainText x)          = x
convertMD' (Italic x)             = tag "em" x
convertMD' (Bold x)               = tag "strong" x
convertMD' (BoldAndItalic x)      = tag "strong" $ tag "em" x
convertMD' (Strikethrough x)      = tag "s" x
convertMD' (Link text url title)  = propTag'' "a" [("href", Just url), ("title", title)] (concat' text)
convertMD' (Image text url title) = propTag' "img" [("src", Just url), ("alt", Just text), ("title", title)]
convertMD' (Code x)               = tag "code" $ escapeHTML x
convertMD' (CodeBlock x)          = tag' "pre" $ tag "code" $ escapeHTML x
convertMD' (Footnote x)           = propTag "sup" [("id", Just ("fnref:" <> x))] $ propTag "a" [("href", Just ("#fn:" <> x))] x
convertMD' (FootnoteRef x xs)     = propTag "li" [("id", Just ("fn:" <> x))] $ concat' xs
convertMD' (FootnoteRefs xs)      = propTag "div" [("id", Just "footnotes")] $ ("<hr/>" <>) $ tag'' "ol" $ concat' xs

concat' :: [MDElem] -> ByteString
concat' = concat'' . (convertMD' <$>)

concat'' :: [ByteString] -> ByteString
concat'' x
  | mconcat x == "" = mconcat x
  | BS.last (mconcat x) == 10 = BS.init $ mconcat x
  | otherwise = mconcat x

tag :: ByteString -> ByteString -> ByteString
tag name x = mconcat ["<", name, ">", x, "</", name, ">"]

tag' :: ByteString -> ByteString -> ByteString
tag' name x = mconcat ["<", name, ">", x, "</", name, ">\n"]

tag'' :: ByteString -> ByteString -> ByteString
tag'' name x = mconcat ["<", name, ">\n", x, "\n</", name, ">\n"]

-- | <x xx="xx">z</x>
propTag :: ByteString -> [(ByteString, Maybe ByteString)] -> ByteString -> ByteString
propTag tagName prop x = mconcat ["<", tagName, " ", mconcat.intersperse " " $ makeProp <$> prop, ">", x, "</", tagName, ">\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

-- | <x xx="xxx"/>
propTag' :: ByteString -> [(ByteString, Maybe ByteString)] -> ByteString
propTag' tagName prop = mconcat ["<", tagName, " ", mconcat.intersperse " " $ makeProp <$> prop, "/>\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

-- | <x xx="xx">z</x>
propTag'' :: ByteString -> [(ByteString, Maybe ByteString)] -> ByteString -> ByteString
propTag'' tagName prop x = mconcat ["<", tagName, " ", mconcat.intersperse " " $ makeProp <$> prop, ">", x, "</", tagName, ">"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

escapeHTML :: ByteString -> ByteString
escapeHTML x = pack $ mconcat $ escapeWord8 <$> unpack x
  where
    escapeWord8 60 = unpack "&lt;"
    escapeWord8 62 = unpack "&gt;"
    escapeWord8 38 = unpack "&amp;"
    escapeWord8 34 = unpack "&quot;"
    escapeWord8 39 = unpack "&#39;"
    escapeWord8 y = [y]

-- Make footnotes be suffixes
takeFn :: [MDElem] -> [MDElem]
takeFn xs =
  let fns = foldr (\a b -> case a of
                              FootnoteRef _ _ -> (Left a) : b
                              _ -> Right a : b) [] xs in
  case lefts fns of
              [] -> rights fns
              lFns -> rights fns <> pure (FootnoteRefs lFns)
