module Article.Query where

import           Data.Map.Lazy                 as M
                                                ( (!?)
                                                , Map
                                                )
import           Data.Text                     as T
                                                ( Text
                                                , unpack
                                                )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeOrError
                                                )

import           Template.Type                  ( ObjectTree(..) )

getNode :: Text -> ObjectTree -> Maybe ObjectTree
getNode key (ObjNode objs) = case objs !? key of
  Just x@(ObjNode _) -> Just x
  _                  -> Nothing
getNode _ _ = Nothing

getLeaf :: Text -> ObjectTree -> Maybe ObjectTree
getLeaf key (ObjNode objs) = case objs !? key of
  Just x@(ObjLeaf _) -> Just x
  _                  -> Nothing
getLeaf _ _ = Nothing

getNode' :: Text -> ObjectTree -> Maybe (Map Text ObjectTree)
getNode' key obj = case getNode key obj of
  Just (ObjNode x) -> Just x
  _                -> Nothing

getLeaf' :: Text -> ObjectTree -> Maybe Text
getLeaf' key obj = case getLeaf key obj of
  Just (ObjLeaf x) -> Just x
  _                -> Nothing

getDate :: ObjectTree -> Maybe UTCTime
getDate obj =
  (\x ->
      parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" (T.unpack x) :: UTCTime
    )
    <$> (getLeaf' "date" obj)

getCategory :: ObjectTree -> Maybe Text
getCategory = getLeaf' "category"
