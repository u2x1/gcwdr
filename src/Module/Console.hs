module Module.Console where

import Data.Template.Type

import Module.Encrypt (encArticle)

runAtclModule :: [ObjectTree] -> [ObjectTree]
runAtclModule obj = fmap (justOrId encArticle) obj

justOrId :: (a -> Maybe a) -> a -> a
justOrId f x = case f x of
                  Just a -> a
                  _ -> x