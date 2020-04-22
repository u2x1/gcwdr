module Template.Convert where

import Template.Type
import Data.Map.Lazy
import Data.ByteString

convertTP' :: ObjectTree -> Stmt -> Maybe ByteString
convertTP' (ObjNode objs) (DotStmt obj mems) = run objs (objs ! obj) mems
  where
    run objs (ObjLeaf _) (x:xs) = Nothing
    run objs obj [] = case obj of
                        ObjLeaf x -> Just x
                        _ -> Nothing
    run objs (ObjNode obj) (x:xs) = run objs (obj ! x) xs
