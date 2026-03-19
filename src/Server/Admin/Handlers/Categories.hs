module Server.Admin.Handlers.Categories where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.List                  ( sortBy )
import           Data.Maybe                 ( fromMaybe )
import           Data.Ord                   ( Down(..), comparing )
import qualified Data.Map.Strict           as MS
import qualified Data.Text                 as T

import           Web.Scotty                 ( ActionM, json )

import           Data.Config.Type           ( Config(..) )
import           Data.Template              ( getLeaf' )
import           Data.Markdown              ( parsePost )
import           Server.Admin.Types
import           Server.Admin.Handlers.Articles ( findMdFiles )

-- | GET /admin/api/categories — list all categories with article counts
listCategories :: AdminEnv -> ActionM ()
listCategories env = do
  let root = articleDir (envConfig env)
  paths <- liftIO $ findMdFiles root
  cats  <- liftIO $ mapM getCategory paths
  let countMap = MS.fromListWith (+) [ (c, 1 :: Int) | c <- cats ]
      sorted   = sortBy (comparing (Down . snd)) (MS.toList countMap)
      result   = [ CategoryInfo name cnt | (name, cnt) <- sorted ]
  json result
  where
    getCategory path = do
      mObj <- parsePost path
      pure $ case mObj of
        Just obj -> fromMaybe "(uncategorized)" (getLeaf' "category" obj)
        Nothing  -> "(uncategorized)" :: T.Text
