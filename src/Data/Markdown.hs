{-# LANGUAGE OverloadedStrings #-}
module Data.Markdown where

import Control.Applicative (many)
import Data.Attoparsec.Text
import Data.List.Extra            as LE (init, dropWhileEnd)
import Data.Text                        (Text)
import qualified Data.Text        as T
import Data.Text.IO               as TL (readFile)
import Data.List                        (intersperse)
import Data.Either                      (rights, lefts)
import Data.Map.Lazy              as M  (singleton, (!?), Map, fromList)
import Data.Maybe

import Data.Markdown.Type
import Data.Markdown.Parser
import Data.Template.Type

parsePost :: FilePath -> IO (Maybe ObjectTree)
parsePost path = do
  s <- TL.readFile path
  case toObjectTree <$> parseOnly post s of
    Right (ObjNode x) -> do
      let relPath = T.toLower (snd $ T.breakOnEnd "content/" $ T.pack $ LE.init $ dropWhileEnd (/='.') path)
      pure $ Just (ObjNode (M.singleton "relLink" (ObjLeaf (relPath <> "/")) <> x))
    _ -> pure Nothing

type MetaData = Map Text ObjectTree
data Post = Post MetaData Text
  deriving (Show)

instance ToObjectTree Post where
  toObjectTree (Post meta content) = ObjNode (meta <> M.singleton "content" (ObjLeaf content))

post :: Parser Post
post = do
  meta' <- metaData
  let meta = if isJust (meta' !? "template")
                then meta'
                else meta' <> M.singleton "template" (ObjLeaf "post")  -- default template to "post"
  postContent <- takeText
  return $ Post meta (convertMD postContent)

metaData :: Parser MetaData
metaData = do
  _ <- many (string "---\n")
  els <- manyTill el (string "---")
  _ <- many (char '\n')
  return (ObjLeaf <$> fromList els)
  where el = do
          obj <- takeTill (== ':') <* char ':' <* many (char ' ')
          text <- takeTill isEndOfLine <* satisfy isEndOfLine
          return (obj, text)

convertMD :: Text -> Text
convertMD s = case parseOnly (many' mdElem) (s <> "\n\n") of
                Right xs -> concat' (takeFn xs)
                _ -> ""

convertMD' :: MDElem -> Text
convertMD' (Header hz x) = tag' ("h" <> T.pack (show hz)) x
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

concat' :: [MDElem] -> Text
concat' = concat'' . (convertMD' <$>)

concat'' :: [Text] -> Text
concat'' x
  | mconcat x == "" = mconcat x
  | T.last (mconcat x) == '\n' = T.init $ mconcat x
  | otherwise = mconcat x

tag :: Text -> Text -> Text
tag name x = mconcat ["<", name, ">", x, "</", name, ">"]

tag' :: Text -> Text -> Text
tag' name x = mconcat ["<", name, ">", x, "</", name, ">\n"]

tag'' :: Text -> Text -> Text
tag'' name x = mconcat ["<", name, ">\n", x, "\n</", name, ">\n"]

-- | <x xx="xx">z</x>
propTag :: Text -> [(Text, Maybe Text)] -> Text -> Text
propTag tagName prop x = mconcat ["<", tagName, " ", mconcat.intersperse " " $ makeProp <$> prop, ">", x, "</", tagName, ">\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

-- | <x xx="xxx"/>
propTag' :: Text -> [(Text, Maybe Text)] -> Text
propTag' tagName prop = mconcat ["<", tagName, " ", mconcat.intersperse " " $ makeProp <$> prop, "/>\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

-- | <x xx="xx">z</x>
propTag'' :: Text -> [(Text, Maybe Text)] -> Text -> Text
propTag'' tagName prop x = mconcat ["<", tagName, " ", mconcat.intersperse " " $ makeProp <$> prop, ">", x, "</", tagName, ">"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]

escapeHTML :: Text -> Text
escapeHTML x = T.pack $ mconcat $ escapeWord8 <$> T.unpack x
  where
    escapeWord8 '<' = T.unpack "&lt;"
    escapeWord8 '>' = T.unpack "&gt;"
    escapeWord8 '&' = T.unpack "&amp;"
    escapeWord8 '\"' = T.unpack "&quot;"
    escapeWord8 '\'' = T.unpack "&#39;"
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