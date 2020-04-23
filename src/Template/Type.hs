module Template.Type where

import Data.ByteString
import Data.Map.Lazy
import Data.Time.Clock

type MapO x = Map ByteString x

data Stmt = DotStmt ByteString [ByteString]
          | ForeachStmt ByteString Stmt [Stmt]
          | PartialStmt ByteString
          | IfStmt
          | Raw ByteString
  deriving (Show)

data ObjectTree = ObjNode (Map ByteString ObjectTree) | ObjLeaf ByteString
  deriving (Show)

data Object = Posts [MapO PageElem] | Pages [MapO PageElem]
  deriving (Show)

data PageElem = Title ByteString | Date UTCTime | Content ByteString
  deriving (Show)
