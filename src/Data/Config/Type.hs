{-# LANGUAGE OverloadedStrings #-}
module Data.Config.Type where

import Data.ByteString     (ByteString)
import Data.Map.Lazy       (fromList)
import Data.Template       (toNodeList)
import Data.Template.Type

data Config = Config {
    siteTitle :: ByteString
  , siteUrl   :: ByteString
  , siteMenus :: [Menu]
} deriving (Show)

data Menu = Menu {
    menuName :: ByteString
  , menuLoc :: ByteString
} deriving (Show)

instance ToObjectTree Menu where
  toObjectTree menu = ObjNode (fromList tup)
    where tup = [ ("menuName", ObjLeaf $ menuName menu)
                , ("menuLoc" , ObjLeaf $ menuLoc menu) ]
  

instance ToObjectTree Config where
  toObjectTree config = ObjNode (fromList tup)
    where tup = [ ("siteTitle", ObjLeaf $ siteTitle config)
                , ("siteUrl"  , ObjLeaf $ siteUrl config)
                , ("siteMenus", toNodeList (toObjectTree <$> (siteMenus config)))]
