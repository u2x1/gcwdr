{-# LANGUAGE OverloadedStrings #-}
module Module.Encrypt where

import Data.Text.Lazy as TL (pack, zip, pack, fromStrict, toStrict)
import Data.Text      as T  (Text)
import Data.Char            (ord, chr)
import Data.Template.Type
import Data.Template
import Data.Map

encArticle :: ObjectTree -> Maybe ObjectTree
encArticle obj = encArticle' obj <$> getLeaf' "passwd" obj

encArticle' :: ObjectTree -> Text -> ObjectTree
encArticle' (ObjNode obj) passwd = ObjNode (update f "content" obj)
  where f (ObjLeaf x) = Just . ObjLeaf $ advance passwd x
        f _ = Nothing
encArticle' x _ = x

advance :: Text -> Text -> Text
advance passwd origin = toStrict $ pack $ fmap (\(a, b) -> charPlus a b) s
  where s = TL.zip (mconcat.repeat $ fromStrict $ passwd) (fromStrict origin)

charPlus :: Char -> Char -> Char
charPlus a b = chr $ (ord a) + (ord b)
