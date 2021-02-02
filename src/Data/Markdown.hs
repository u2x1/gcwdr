{-# LANGUAGE OverloadedStrings #-}
module Data.Markdown where

import           Control.Applicative            ( many )
import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , isEndOfLine
                                                , many'
                                                , manyTill
                                                , parseOnly
                                                , satisfy
                                                , string
                                                , takeText
                                                , takeTill
                                                )
import           Data.Either                    ( fromRight
                                                , lefts
                                                , rights
                                                )
import           Data.List                      ( intersperse )
import           Data.List.Extra               as LE
                                                ( dropWhileEnd
                                                , init
                                                )
import           Data.Map.Lazy                 as M
                                                ( (!?)
                                                , Map
                                                , fromList
                                                , insert
                                                , update
                                                )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                  as TL
                                                ( readFile )

import           Data.Markdown.Parser           ( mdElem )
import           Data.Markdown.Type             ( MDElem(..) )
import           Data.Template.Type             ( ObjectTree(ObjLeaf, ObjNode)
                                                , ToObjectTree(..)
                                                )

markdown2Html :: Text -> Text
markdown2Html = mdElems2Html . text2MDElems

parsePost :: FilePath -> IO (Maybe ObjectTree)
parsePost path = do
  s <- TL.readFile path
  case toObjectTree <$> parseOnly post s of
    Right (ObjNode x) -> do
      let relPath = T.toLower
            (snd $ T.breakOnEnd "content/" $ T.pack $ LE.init $ dropWhileEnd
              (/= '.')
              path
            )
      pure $ Just (ObjNode (M.insert "relLink" (ObjLeaf (relPath <> "/")) x))
    _ -> pure Nothing

post :: Parser Post
post = do
  meta'       <- metaData
  postMDElems <- text2MDElems <$> takeText
  let content = mdElems2Html postMDElems
      outline = getOutline postMDElems
      meta = insert' "outline" outline $ if isJust (meta' !? "template")
        then meta'
        else M.insert "template" (ObjLeaf "post") meta'  -- default template to "post"
  return $ Post meta content
 where
  insert' _   ""           = id
  insert' key txt = M.insert key (ObjLeaf txt)

metaData :: Parser MetaData
metaData = do
  _   <- many (string "---\n")
  els <- manyTill el (string "---")
  _   <- many (char '\n')
  return (ObjLeaf <$> fromList els)
 where
  el = do
    obj  <- takeTill (== ':') <* char ':' <* many (char ' ')
    text <- takeTill isEndOfLine <* satisfy isEndOfLine
    return (obj, text)

text2MDElems :: Text -> [MDElem]
text2MDElems x =
  idHdr 0 . moveFn2Bottom $ rawMdElem
  where
    rawMdElem = fromRight [] $ parseOnly (many' mdElem) (x <> "\n\n")
    moveFn2Bottom xs =
      let fns = map
            (\a ->
              case a of
                  FootnoteRef{} -> Left a
                  _             -> Right a)
            xs
      in rights fns <> pure (FootnoteRefs $ lefts fns)
    idHdr i (hdr@(Header a b _):xs) = Header a b i : idHdr  (i + 1) xs
    idHdr i (x:xs) = x : idHdr i xs
    idHdr i [] = []
 

mdElems2Html :: [MDElem] -> Text
mdElems2Html = mconcat . fmap mdElem2Html

mdElem2Html :: MDElem -> Text
mdElem2Html (Header hz x hid) = propTag ("h" <> T.pack (show hz))
                                        [("id", headerIdText)]
                                        x
  where headerIdText = Just . T.pack $ "hdr:" <> show hid
mdElem2Html (Paragrah      xs) = tag' "p" $ mdElems2Html xs
mdElem2Html (Blockquotes   xs) = tag'' "blockquote" $ mdElems2Html xs
mdElem2Html (OrderedList   xs) = tag'' "ol" $ mdElems2Html xs
mdElem2Html (UnorderedList xs) = tag'' "ul" $ mdElems2Html xs
mdElem2Html (ListElem      xs) = tag "li" $ mdElems2Html xs
mdElem2Html (PlainText     x ) = x
mdElem2Html HorizontalRule     = "<hr>\n"
mdElem2Html (Italic        x)  = tag "em" x
mdElem2Html (Bold          x)  = tag "strong" x
mdElem2Html (BoldAndItalic x)  = tag "strong" $ tag "em" x
mdElem2Html (Strikethrough x)  = tag "s" x
mdElem2Html (Link text url title) =
  propTag'' "a" [("href", Just url), ("title", title)] (mdElems2Html text)
mdElem2Html (Image text url title) =
  propTag' "img" [("src", Just url), ("alt", Just text), ("title", title)]
mdElem2Html (Code      x) = tag "code" $ escapeHTML x
mdElem2Html (CodeBlock x) = tag' "pre" $ tag "code" $ escapeHTML x
mdElem2Html (Footnote x) =
  propTag "sup" [("id", Just ("fnref:" <> x))]
    $ propTag "a" [("href", Just ("#fn:" <> x))] x
mdElem2Html (FootnoteRef x xs) =
  propTag "li" [("id", Just ("fn:" <> x))] $ mdElems2Html xs
mdElem2Html (FootnoteRefs []) = ""
mdElem2Html (FootnoteRefs xs) =
  propTag "div" [("id", Just "footnotes")]
    $ ("<hr/>" <>)
    $ tag'' "ol"
    $ mdElems2Html xs


tag :: Text -> Text -> Text
tag name x = mconcat ["<", name, ">", x, "</", name, ">"]

tag' :: Text -> Text -> Text
tag' name x = mconcat ["<", name, ">", x, "</", name, ">\n"]

tag'' :: Text -> Text -> Text
tag'' name x = mconcat ["<", name, ">\n", x, "\n</", name, ">\n"]

-- | <x xx="xx">z</x>
propTag :: Text -> [(Text, Maybe Text)] -> Text -> Text
propTag tagName prop x = mconcat
  [ "<"
  , tagName
  , " "
  , mconcat . intersperse " " $ makeProp <$> prop
  , ">"
  , x
  , "</"
  , tagName
  , ">\n"
  ]
 where
  makeProp (_   , Nothing   ) = ""
  makeProp (name, Just value) = mconcat [name, "=\"", value, "\""]

-- | <x xx="xxx"/>
propTag' :: Text -> [(Text, Maybe Text)] -> Text
propTag' tagName prop = mconcat
  ["<", tagName, " ", mconcat . intersperse " " $ makeProp <$> prop, "/>\n"]
 where
  makeProp (_   , Nothing   ) = ""
  makeProp (name, Just value) = mconcat [name, "=\"", value, "\""]

-- | <x xx="xx">z</x>
propTag'' :: Text -> [(Text, Maybe Text)] -> Text -> Text
propTag'' tagName prop x = mconcat
  [ "<"
  , tagName
  , " "
  , mconcat . intersperse " " $ makeProp <$> prop
  , ">"
  , x
  , "</"
  , tagName
  , ">"
  ]
 where
  makeProp (_   , Nothing   ) = ""
  makeProp (name, Just value) = mconcat [name, "=\"", value, "\""]

escapeHTML :: Text -> Text
escapeHTML x = T.pack $ mconcat $ escapeWord8 <$> T.unpack x
 where
  escapeWord8 '<'  = T.unpack "&lt;"
  escapeWord8 '>'  = T.unpack "&gt;"
  escapeWord8 '&'  = T.unpack "&amp;"
  escapeWord8 '\"' = T.unpack "&quot;"
  escapeWord8 '\'' = T.unpack "&#39;"
  escapeWord8 y    = [y]

getOutline :: [MDElem] -> Text
getOutline elems = go [] headers

 where
  headers = filter isHeader elems
  isHeader Header{} = True
  isHeader _        = False

  go :: [Int] -> [MDElem] -> Text
  go depths ((Header hdSz hdTx hid) : xs) =
    let closure  = length $ takeWhile (> hdSz) depths
        newDepth = if null depths || hdSz > head depths
          then hdSz : depths
          else drop closure depths
        prefix | closure > 0        = mconcat (replicate closure "</ul>\n")
               | newDepth == depths = ""
               | otherwise          = "<ul>\n\t"
        suffix | null xs   = mconcat $ replicate (length depths + 1) "</ul>\n"
               | otherwise = ""
    in  prefix
          <> "<li><a href=\"#hdr:"
          <> T.pack (show hid)
          <> "\">"
          <> hdTx
          <> "</a></li>\n"
          <> suffix
          <> go newDepth xs
  go _ _ = ""


type MetaData = Map Text ObjectTree

data Post = Post MetaData Text
  deriving Show

instance ToObjectTree Post where
  toObjectTree (Post meta content) =
    ObjNode (M.insert "content" (ObjLeaf content) meta)