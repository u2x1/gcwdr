{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Server.Admin where

import Web.Scotty
import Data.Markdown ( markdown2Html )
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy.Char8 ( fromStrict )
import Data.Text (Text)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson ( FromJSON, ToJSON )
import System.Directory ( createDirectory, doesDirectoryExist )
import Network.HTTP.Types ()
import qualified Data.Text.IO as T
import GHC.Generics ( Generic )
import Data.List ( dropWhileEnd )
import System.FilePath.Find
    ( (&&?),
      (==?),
      always,
      extension,
      fileType,
      find,
      FileType(RegularFile) )
import Server.Preview ( preview )

adminServer :: Int -> FilePath -> FilePath -> IO ()
adminServer port root adminRoot = scotty port $ do
    post "/api/mdPreview/"      (cors >> previewMarkdown)
    get  "/api/getArticleList/" (cors >> (getArticleList root))
    post "/api/getArticleData/" (cors >> getArticleData)
    post "/api/updateArticle/"  (cors >> updateArticle)
    preview adminRoot
    where
        cors = setHeader "Access-Control-Allow-Origin" "*"

previewMarkdown :: ActionM ()
previewMarkdown = do
    mdTxt <- body
    raw $ fromStrict $ T.encodeUtf8 $ markdown2Html $ TL.toStrict $ TL.decodeUtf8 mdTxt

updateArticle :: ActionM ()
updateArticle = do
    postData <- jsonData :: ActionM ArticleData
    let path = post_path postData
        dir = dropWhileEnd (/='/') path
    dirExist <- liftIO $ doesDirectoryExist dir
    if dirExist then pure () else liftIO $ createDirectory dir
    liftIO $ T.writeFile path (post_raw_content postData)

getArticleList :: FilePath -> ActionM ()
getArticleList root = do
    paths <- liftIO $
      find always (fileType ==? RegularFile &&? extension ==? ".md") root
    json paths

getArticleData :: ActionM ()
getArticleData = do
    content <- (liftIO . LB.readFile .LB.unpack) =<< body
    raw content

data ArticleData = ArticleData {
    post_path :: FilePath,     -- /post/some-article.md
    post_raw_content :: Text
} deriving Generic
instance FromJSON ArticleData
instance ToJSON ArticleData