module Markdown.Type where

import Data.ByteString

data MDElem = Header1             ByteString
            | Header2             ByteString
            | Header3             ByteString
            | Header4             ByteString
            | Header5             ByteString
            | Header6             ByteString
            | Header7             ByteString
            | Paragrah            [MDElem]    -- Should contain Italic, Bold, BoldAndItalic, Code
                                              --                                    , Link, Image
            | PlainText           ByteString
            | Italic              ByteString
            | Bold                ByteString
            | BoldAndItalic       ByteString
            | Strikethrough       ByteString
            | Code                ByteString
            | CodeBlock           ByteString
            | Link                ByteString ByteString (Maybe ByteString)
            | Image               ByteString ByteString (Maybe ByteString)
            | OrderedList         [MDElem]
            | UnorderedList       [MDElem]
            | ListElem     ByteString
            | Blockquotes         [MDElem]
            | HorizontalRule
  deriving (Show)
