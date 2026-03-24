module Article.Transform where

import           Data.Map.Lazy                 as M
                                                ( Map
                                                , insert
                                                , singleton
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )

import           Template.Type                  ( ObjectTree(..) )
import           Article.Encrypt                ( encArticle )

singletonObjNode :: [Text] -> Map Text ObjectTree -> Map Text ObjectTree
singletonObjNode []       = id
singletonObjNode (y : ys) = (singleton y) . ObjNode . (singletonObjNode ys)

addGlb :: Map Text ObjectTree -> ObjectTree -> ObjectTree
addGlb glbRes x = ObjNode (M.insert "this" x glbRes)

toNodeList :: [ObjectTree] -> ObjectTree
toNodeList = ObjNodeList . catMaybes . fmap extract
  where extract (ObjNode x) = Just x
        extract _ = Nothing

runAtclModule :: [ObjectTree] -> [ObjectTree]
runAtclModule = fmap (justOrId encArticle)

justOrId :: (a -> Maybe a) -> a -> a
justOrId f x = case f x of
  Just a -> a
  _      -> x
