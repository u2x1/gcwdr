module Article.Parse where

import           Control.Applicative            ( many, some )
import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , isEndOfLine
                                                , manyTill
                                                , parseOnly
                                                , satisfy
                                                , string
                                                , takeText
                                                , takeTill
                                                )
import           Data.List.Extra               as LE
                                                ( dropWhileEnd
                                                , init
                                                )
import           Data.Map.Lazy                 as M
                                                ( (!?)
                                                , fromList
                                                , insert
                                                )
import           Data.Maybe                     ( isJust )
import qualified Data.Text                     as T
import           Data.Text.IO                  as TL
                                                ( readFile )

import           Article.Type                   ( MetaData
                                                , Post(..)
                                                )
import           Markdown.Render                ( getOutline
                                                , mdElems2Html
                                                , text2MDElems
                                                )
import           Template.Type                  ( ObjectTree(ObjLeaf, ObjNode)
                                                , ToObjectTree(..)
                                                )

parsePost :: FilePath -> IO (Either String ObjectTree)
parsePost path = do
  s <- TL.readFile path
  case toObjectTree <$> parseOnly post s of
    Right (ObjNode x) -> do
      let relPath = T.toLower
            (snd $ T.breakOnEnd "content/" $ T.pack $ LE.init $ dropWhileEnd
              (/= '.')
              path
            )
      pure $ Right (ObjNode (M.insert "relLink" (ObjLeaf ("/" <> relPath <> "/")) x))
    Right _ -> pure $ Left $ "parsed article from " <> path <> " is not an ObjNode"
    Left err -> pure $ Left $ "failed to parse article " <> path <> ": " <> err

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
  _   <- many (string "---") <* some (satisfy isEndOfLine)
  els <- manyTill el (string "---")
  _   <- many (satisfy isEndOfLine)
  return (ObjLeaf <$> fromList els)
 where
  el = do
    obj  <- takeTill (== ':') <* char ':' <* many (char ' ')
    text <- takeTill isEndOfLine <* (many (satisfy isEndOfLine))
    return (obj, text)
