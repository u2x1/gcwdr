module Article.Type where

import           Data.Map.Lazy                 as M
                                                ( Map
                                                , insert
                                                )
import           Data.Text                      ( Text )

import           Template.Type                  ( ObjectTree(ObjLeaf, ObjNode)
                                                , ToObjectTree(..)
                                                )

type MetaData = Map Text ObjectTree

data Post = Post MetaData Text
  deriving Show

instance ToObjectTree Post where
  toObjectTree (Post meta content) =
    ObjNode (M.insert "content" (ObjLeaf content) meta)
