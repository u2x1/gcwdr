module Markdown.Render where

import           Data.Attoparsec.Text           ( many'
                                                , parseOnly
                                                )
import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Data.List                      ( intersperse )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Markdown.Parser                ( mdElem )
import           Markdown.Highlight             ( highlightCode )
import           Markdown.Type                  ( MDElem(..) )

markdown2Html :: Text -> Text
markdown2Html = mdElems2Html . text2MDElems

text2MDElems :: Text -> [MDElem]
text2MDElems text =
  idHdr 0 . moveFn2Bottom $ rawMdElem
  where
    rawMdElem = case parseOnly (many' mdElem) (text <> "\n\n") of
      Right xs -> xs
      Left  err -> [PlainText $ T.pack $ "Markdown parse error: " <> err]
    moveFn2Bottom xs =
      let fns = map
            (\a ->
              case a of
                  FootnoteRef{} -> Left a
                  _             -> Right a)
            xs
      in rights fns <> pure (FootnoteRefs $ lefts fns)
    idHdr i ((Header a b _):xs) = Header a b i : idHdr  (i + 1) xs
    idHdr i (x:xs) = x : idHdr i xs
    idHdr _ [] = []


mdElems2Html :: [MDElem] -> Text
mdElems2Html = mconcat . fmap mdElem2Html

mdElem2Html :: MDElem -> Text
mdElem2Html (Header hz x hid) = propTag ("h" <> T.pack (show hz))
                                        [("id", headerIdText)]
                                        x
  where headerIdText = Just . T.pack $ "hdr:" <> show hid
mdElem2Html (Paragrah      xs) = tag' "p" $ mdElems2Html xs
mdElem2Html (Blockquotes   xs) = tag'' "blockquote" $ mdElems2Html xs
mdElem2Html (ExpandBlock   title xs) =
   tag'' "details" $
    ((tag' "summary" title) <> (mdElems2Html xs))
mdElem2Html (OrderedList   xs) = tag'' "ol" $ mdElems2Html xs
mdElem2Html (UnorderedList xs) = tag'' "ul" $ mdElems2Html xs
mdElem2Html (ListElem      xs) = tag "li" $ mdElems2Html xs
mdElem2Html (PlainText     x ) = x
mdElem2Html (LatexInline   x ) = "$" <> x <> "$"
mdElem2Html (LatexBlock    x ) = "$$" <> x <> "$$"
mdElem2Html HorizontalRule     = "<hr>\n"
mdElem2Html (Italic        x)  = tag "em" x
mdElem2Html (Bold          x)  = tag "strong" x
mdElem2Html (BoldAndItalic x)  = tag "strong" $ tag "em" x
mdElem2Html (Strikethrough x)  = tag "s" x
mdElem2Html (Link text url title) =
  propTag'' "a" [("href", Just url), ("title", title)] (mdElems2Html text)
mdElem2Html (Image text url title) =
  case title of
    Just caption ->
      tag'' "figure" $
        propTag' "img" [("src", Just url), ("alt", Just text), ("title", Just caption)]
        <> tag' "figcaption" caption
    Nothing ->
      propTag' "img" [("src", Just url), ("alt", Just text), ("title", Nothing)]
mdElem2Html (Code      x) = tag "code" $ escapeHTML x
mdElem2Html (CodeBlock lang body) = highlightCode lang body
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
mdElem2Html (RawHtmlTag name props content) = rawTag name props content

rawTag :: Text -> Text -> Text -> Text
rawTag name props content = mconcat ["<", name, " ", props, ">", content, "</", name, ">"]

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
        newDepth = case depths of
          []    -> hdSz : depths
          (d:_) -> if hdSz > d
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
