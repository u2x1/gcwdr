module Data.Markdown.Type where

import           Data.Text                      ( Text )

data MDElem = Header              Int Text Int -- The third Int is for href '#hdr:'
            | Paragrah            [MDElem]    -- Should contain Italic, Bold, BoldAndItalic, Code
                                              --                                    , Link, Image
            | PlainText           Text
            | Italic              Text
            | Bold                Text
            | LatexInline         Text
            | LatexBlock          Text
            | BoldAndItalic       Text
            | Strikethrough       Text
            | Code                Text
            | CodeBlock           Text
            | ExpandBlock         Text [MDElem]
            | Link                [MDElem] Text (Maybe Text)
            | Image               Text Text (Maybe Text)
            | OrderedList         [MDElem]    -- Should contain ListElem
            | UnorderedList       [MDElem]    -- As above
            | ListElem            [MDElem]    -- Should contain the same as Paragrah contains
            | Blockquotes         [MDElem]    -- Should contain the whole fuckin universe
            | Footnote            Text
            | FootnoteRef         Text [MDElem]
            | FootnoteRefs        [MDElem]
            | HorizontalRule
            | RawHtmlTag          Text Text Text
  deriving (Show, Eq)
