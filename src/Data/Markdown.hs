{-# LANGUAGE OverloadedStrings #-}
module Data.Markdown where

import Control.Applicative (many)
import Data.Attoparsec.Text
import Data.List.Extra            as LE (init, dropWhileEnd)
import Data.Text                        (Text)
import qualified Data.Text        as T
import Data.Text.IO               as TL (readFile)
import Data.List                        (intersperse)
import Data.Either                      (rights, lefts, fromRight)
import Data.Map.Lazy              as M  (insert, (!?), Map, fromList, update)
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
      pure $ Just (ObjNode (M.insert "relLink" (ObjLeaf (relPath <> "/")) x))
    _ -> pure Nothing

type MetaData = Map Text ObjectTree
data Post = Post MetaData Text
  deriving (Show)

instance ToObjectTree Post where
  toObjectTree (Post meta content) = ObjNode (M.insert "content" (ObjLeaf content) meta)

post :: Parser Post
post = do
  meta' <- metaData
  postMDElems <- text2MDElem <$> takeText
  let postHtml = convertMD postMDElems
  let outline = getOutlines postMDElems
  let meta = insert' "outline" outline $
               if isJust (meta' !? "template")
                  then meta'
                  else M.insert "template" (ObjLeaf "post") meta'  -- default template to "post"
  return $ Post meta postHtml
  where insert' key (Just leaf) = M.insert key (ObjLeaf leaf)
        insert' _ _ = id

getOutlines :: [MDElem] -> Maybe Text
getOutlines elems = let x = go [] headers 0 in
                    if T.null x then Nothing else Just x
  where headers = filter isHeader elems
        isHeader (Header _ _) = True
        isHeader _ = False
        go :: [Int] -> [MDElem] -> Int -> Text
        go [] [] _ = ""
        go [] ((Header hdSz hdTx) : xs) ct = "<ul>\n" <> "<li><a href=\"#hdr:" <> T.pack (show ct) <> "\">" <> hdTx <> "</a></li>\n" <>
                                              go [hdSz] xs (ct + 1)
        go depth ((Header hdSz hdTx) : xs) ct = 
          let closure  = (length $ Prelude.takeWhile (> hdSz) depth)
              newDepth = if hdSz > (head depth)
                            then hdSz : depth
                            else drop closure depth
              prefix = if closure > 0
                          then mconcat (replicate closure "</ul>\n")
                          else if newDepth == depth then "" else "<ul>\n" in
          prefix <> "<li><a href=\"#hdr:" <> T.pack (show ct) <> "\">" <> hdTx <> "</a></li>\n" <>
            go newDepth xs (ct + 1) 
        go _ _ _ = ""  -- should never be called

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

convertMD :: [MDElem] -> Text
convertMD elems = let counter = fromList [("header", 0)] in
                  mdElems2Text counter elems

text2MDElem :: Text -> [MDElem]                
text2MDElem x = fromRight [] $ takeFn <$> parseOnly (many' mdElem) (x <> "\n\n")

convertMD' :: Map String Int-> MDElem -> ((Map String Int), Text)
convertMD' counter (Header hz x)          = (,) updatedCounter $ propTag ("h" <> T.pack (show hz)) [("id", headerIdText)] x
  where headerIdText   = T.pack <$> ((<>) <$> (Just "hdr:") <*> (show <$> (counter !? "header")))
        updatedCounter = update (Just . (+1)) "header" counter
convertMD' counter (Paragrah xs)          = (,) counter $ tag'  "p"           $ mdElems2Text counter xs
convertMD' counter (Blockquotes xs)       = (,) counter $ tag'' "blockquote"  $ mdElems2Text counter xs
convertMD' counter (OrderedList xs)       = (,) counter $ tag'' "ol"          $ mdElems2Text counter xs
convertMD' counter (UnorderedList xs)     = (,) counter $ tag'' "ul"          $ mdElems2Text counter xs
convertMD' counter (ListElem xs)          = (,) counter $ tag   "li"          $ mdElems2Text counter xs
convertMD' counter (PlainText x)          = (,) counter $ x
convertMD' counter HorizontalRule         = (,) counter $ "<hr>\n"
convertMD' counter (Italic x)             = (,) counter $ tag "em" x
convertMD' counter (Bold x)               = (,) counter $ tag "strong" x
convertMD' counter (BoldAndItalic x)      = (,) counter $ tag "strong" $ tag "em" x
convertMD' counter (Strikethrough x)      = (,) counter $ tag "s" x
convertMD' counter (Link text url title)  = (,) counter $ propTag'' "a" [("href", Just url), ("title", title)] (mdElems2Text counter text)
convertMD' counter (Image text url title) = (,) counter $ propTag' "img" [("src", Just url), ("alt", Just text), ("title", title)]
convertMD' counter (Code x)               = (,) counter $ tag "code" $ escapeHTML x
convertMD' counter (CodeBlock x)          = (,) counter $ tag' "pre" $ tag "code" $ escapeHTML x
convertMD' counter (Footnote x)           = (,) counter $ propTag "sup" [("id", Just ("fnref:" <> x))] $ propTag "a" [("href", Just ("#fn:" <> x))] x
convertMD' counter (FootnoteRef x xs)     = (,) counter $ propTag "li" [("id", Just ("fn:" <> x))] $ mdElems2Text counter xs
convertMD' counter (FootnoteRefs xs)      = (,) counter $ propTag "div" [("id", Just "footnotes")] $ ("<hr/>" <>) $ tag'' "ol" $ mdElems2Text counter xs

mdElems2Text :: Map String Int -> [MDElem] -> Text
mdElems2Text counter elems = snd $ foldl (\(counter', txt) e -> (mdfSnd (txt <>) (convertMD' counter' e))) (counter, "") elems
  where mdfSnd f (x, y) = (x, f y)

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

-- move footnotes to bottom
takeFn :: [MDElem] -> [MDElem]
takeFn xs =
  let fns = foldr (\a b -> case a of
                              FootnoteRef _ _ -> (Left a) : b
                              _ -> Right a : b) [] xs in
  case lefts fns of
              [] -> rights fns
              lFns -> rights fns <> pure (FootnoteRefs lFns)