module Markdown.Type where

import Data.ByteString

data MDElem = Header              Int ByteString
            | Paragrah            [MDElem]    -- Should contain Italic, Bold, BoldAndItalic, Code
                                              --                                    , Link, Image
            | PlainText           ByteString
            | Italic              ByteString
            | Bold                ByteString
            | BoldAndItalic       ByteString
            | Strikethrough       ByteString
            | Code                ByteString
            | CodeBlock           ByteString
            | Link                [MDElem] ByteString (Maybe ByteString)
            | Image               ByteString ByteString (Maybe ByteString)
            | OrderedList         [MDElem]    -- Should contain ListElem
            | UnorderedList       [MDElem]    -- As above
            | ListElem            [MDElem]    -- Should contain the same as Paragrah contains
            | Blockquotes         [MDElem]    -- Should contain the whole fuckin universe
            | Footnote            ByteString
            | FootnoteRef         ByteString [MDElem]
            | FootnoteRefs        [MDElem]
            | HorizontalRule
  deriving (Show, Eq)
