module Module.Console where

import           Data.Template.Type             ( ObjectTree )

import           Module.Encrypt                 ( encArticle )

runAtclModule :: [ObjectTree] -> [ObjectTree]
runAtclModule = fmap (justOrId encArticle)

justOrId :: (a -> Maybe a) -> a -> a
justOrId f x = case f x of
  Just a -> a
  _      -> x
