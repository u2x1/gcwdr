{-# LANGUAGE OverloadedStrings #-}
module Data.Template.Type where

import Data.Text as T
import Data.Map.Lazy

type MapO x = Map Text x

data Stmt = DotStmt [Text]
          | ForeachStmt Text Stmt [Stmt]
          | PartialStmt Text
          | IfStmt
          | Raw Text
  deriving (Show, Eq)

data ObjectTree = ObjNode (Map Text ObjectTree)
                | ObjLeaf Text
                | ObjListNode [Map Text ObjectTree]
instance Show ObjectTree where
  show (ObjLeaf x) = unpack x
  show (ObjListNode xss) = mconcat $ fmap (\xs -> Prelude.unlines $ fmap (\(x, y) -> unpack x <> ": " <> show y) (toList xs)) xss
  show (ObjNode xs) = Prelude.unlines $ fmap (\(x, y) -> unpack x <> ":  " <> show y) (toList xs)

class ToObjectTree a where
  toObjectTree :: a -> ObjectTree