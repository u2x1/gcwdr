module Data.Config.Type where

import           Data.Map.Lazy                  ( fromList )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Toml.Schema                    ( FromValue(..)
                                                , parseTableFromValue
                                                , reqKey
                                                )

import           Data.Template                  ( toNodeList )
import           Data.Template.Type             ( ObjectTree(ObjLeaf, ObjNode)
                                                , ToObjectTree(..)
                                                )

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

instance FromValue Config where
  fromValue = parseTableFromValue $
    Config
      <$> reqKey "title"
      <*> reqKey "url"
      <*> reqKey "menu"
      <*> (T.unpack <$> reqKey "outputDir")
      <*> (T.unpack <$> reqKey "themeDir")
      <*> (T.unpack <$> reqKey "articleDir")
      <*> reqKey "localServerPort"

instance FromValue Menu where
  fromValue = parseTableFromValue $
    Menu
      <$> reqKey "name"
      <*> reqKey "loc"

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
