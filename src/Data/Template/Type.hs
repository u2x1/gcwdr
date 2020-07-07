module Data.Template.Type where

import Data.ByteString
import Data.Map.Lazy

type MapO x = Map ByteString x

data Stmt = DotStmt ByteString [ByteString]
          | ForeachStmt ByteString Stmt [Stmt]
          | PartialStmt ByteString
          | IfStmt
          | Raw ByteString
  deriving (Show)

data ObjectTree = ObjNode (Map ByteString ObjectTree)
                | ObjLeaf ByteString
                | ObjListNode [Map ByteString ObjectTree]
  deriving (Show)
