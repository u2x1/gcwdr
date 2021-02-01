{-# LANGUAGE OverloadedStrings #-}
module Data.Config.Type where

import           Data.Map.Lazy                  ( fromList )
import           Data.Template                  ( toNodeList )
import           Data.Template.Type             ( ObjectTree(ObjLeaf, ObjNode)
                                                , ToObjectTree(..)
                                                )
import           Data.Text                      ( Text )

data Config = Config
  { siteTitle       :: Text
  , siteUrl         :: Text
  , siteMenus       :: [Menu]
  , outputDir       :: FilePath
  , themeDir        :: FilePath
  , articleDir      :: FilePath
  , localServerPort :: Int
  }
  deriving Show

data Menu = Menu
  { menuName :: Text
  , menuLoc  :: Text
  }
  deriving Show

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
