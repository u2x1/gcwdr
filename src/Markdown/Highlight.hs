module Markdown.Highlight (highlightCode) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Debug.Trace                    ( trace )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import           Skylighting                    ( TokenizerConfig(..)
                                                , tokenize
                                                , lookupSyntax
                                                , defaultSyntaxMap
                                                , defaultFormatOpts
                                                , formatHtmlBlock
                                                )

highlightCode :: Maybe Text -> Text -> Text
highlightCode Nothing body = plainBlock body
highlightCode (Just lang) body
  | lang `elem` ["text", "plain"] = plainBlock body
  | otherwise = case lookupSyntax lang defaultSyntaxMap of
      Nothing -> trace ("[Warning] unknown syntax: " <> T.unpack lang) $ plainBlock body
      Just syn -> case tokenize config syn body of
        Left err -> trace ("[Warning] tokenizing " <> T.unpack lang <> ": " <> err) $ plainBlock body
        Right srcLines -> TL.toStrict $ renderHtml $ formatHtmlBlock defaultFormatOpts srcLines
 where
  config = TokenizerConfig { syntaxMap = defaultSyntaxMap, traceOutput = False }

plainBlock :: Text -> Text
plainBlock b = "<pre><code>" <> escapeHTML b <> "</code></pre>\n"

escapeHTML :: Text -> Text
escapeHTML x = T.pack $ mconcat $ escapeWord8 <$> T.unpack x
 where
  escapeWord8 '<'  = T.unpack "&lt;"
  escapeWord8 '>'  = T.unpack "&gt;"
  escapeWord8 '&'  = T.unpack "&amp;"
  escapeWord8 '\"' = T.unpack "&quot;"
  escapeWord8 '\'' = T.unpack "&#39;"
  escapeWord8 y    = [y]
