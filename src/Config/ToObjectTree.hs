{-# OPTIONS_GHC -Wno-orphans #-}
module Config.ToObjectTree where

import           Data.Map.Lazy                  ( fromList )

import           Config.Type                    ( Config(..)
                                                , Menu(..)
                                                )
import           Template.Type                  ( ObjectTree(ObjLeaf, ObjNode)
                                                , ToObjectTree(..)
                                                )
import           Article.Transform              ( toNodeList )

instance ToObjectTree Menu where
  toObjectTree menu = ObjNode (fromList tup)
   where
    tup =
      [ ("menuName", ObjLeaf $ menuName menu)
      , ("menuLoc" , ObjLeaf $ menuLoc menu)
      ]

instance ToObjectTree Config where
  toObjectTree config = ObjNode (fromList tup)
   where
    tup =
      [ ("siteTitle", ObjLeaf $ siteTitle config)
      , ("siteUrl"  , ObjLeaf $ siteUrl config)
      , ("siteMenus", toNodeList (toObjectTree <$> siteMenus config))
      ]
