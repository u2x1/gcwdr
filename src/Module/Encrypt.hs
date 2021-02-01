{-# LANGUAGE OverloadedStrings #-}
module Module.Encrypt where

import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Map                       ( update )
import           Data.Template                  ( getLeaf' )
import           Data.Template.Type             ( ObjectTree(ObjLeaf, ObjNode) )
import           Data.Text                     as T
                                                ( Text )
import           Data.Text.Lazy                as TL
                                                ( fromStrict
                                                , pack
                                                , toStrict
                                                , zip
                                                )

encArticle :: ObjectTree -> Maybe ObjectTree
encArticle obj = encArticle' obj <$> getLeaf' "passwd" obj

encArticle' :: ObjectTree -> Text -> ObjectTree
encArticle' (ObjNode obj) passwd = ObjNode (update f "content" obj)
 where
  f (ObjLeaf x) = Just . ObjLeaf $ advance passwd x
  f _           = Nothing
encArticle' x _ = x

advance :: Text -> Text -> Text
advance passwd origin = toStrict $ pack $ fmap (uncurry charPlus) s
  where s = TL.zip (mconcat . repeat $ fromStrict passwd) (fromStrict origin)

charPlus :: Char -> Char -> Char
charPlus a b = chr $ ord a + ord b
