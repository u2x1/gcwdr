module Server.Admin.Handlers.Articles where

import           Control.Monad              ( filterM, when, unless )
import           Control.Monad.IO.Class     ( liftIO )
import           Data.List                  ( isSuffixOf )
import           Data.Maybe                 ( fromMaybe )
import qualified Data.Text.IO              as TIO
import           System.Directory           ( listDirectory, doesFileExist
                                            , doesDirectoryExist
                                            , createDirectoryIfMissing
                                            , renameFile )
import           System.FilePath            ( (</>), takeDirectory )

import           Network.HTTP.Types.Status  ( status404, status409 )
import           Web.Scotty                 ( ActionM, json, jsonData, text
                                            , queryParam, status, finish )

import           Config.Type                ( Config(..) )
import           Article.Query              ( getLeaf' )
import           Template.Type              ( ObjectTree(..) )
import           Article.Parse              ( parsePost )
import           Server.Admin.Types

-- | Recursively find all .md files under a directory
findMdFiles :: FilePath -> IO [FilePath]
findMdFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    let paths = map (dir </>) entries
    files <- filterM doesFileExist paths
    dirs  <- filterM doesDirectoryExist paths
    let mdFiles  = filter (".md" `isSuffixOf`) files
    subFiles <- concat <$> mapM findMdFiles dirs
    pure (mdFiles ++ subFiles)

-- | Extract metadata from a parsed ObjectTree
extractMeta :: FilePath -> ObjectTree -> ArticleMeta
extractMeta path obj = ArticleMeta
  { amPath     = path
  , amTitle    = fromMaybe "" $ getLeaf' "title" obj
  , amDate     = fromMaybe "" $ getLeaf' "date" obj
  , amCategory = fromMaybe "" $ getLeaf' "category" obj
  , amTemplate = fromMaybe "post" $ getLeaf' "template" obj
  }

-- | GET /admin/api/articles — list all articles with metadata
listArticles :: AdminEnv -> ActionM ()
listArticles env = do
  let root = articleDir (envConfig env)
  paths <- liftIO $ findMdFiles root
  metas <- liftIO $ mapM parseMeta paths
  json [ m | Just m <- metas ]
  where
    parseMeta path = do
      mObj <- parsePost path
      case mObj of
        Just obj -> pure $ Just $ extractMeta path obj
        Nothing  -> pure $ Just $ ArticleMeta path "" "" "" "post"

-- | GET /admin/api/articles/get?path=:path — get full article
getArticle :: AdminEnv -> ActionM ()
getArticle _env = do
  path <- queryParam "path"
  exists <- liftIO $ doesFileExist path
  unless exists $ do
    status status404
    text "Article not found"
    finish
  rawContent <- liftIO $ TIO.readFile path
  mObj <- liftIO $ parsePost path
  let meta = case mObj of
        Just obj -> extractMeta path obj
        Nothing  -> ArticleMeta path "" "" "" "post"
  json $ ArticleFull meta rawContent

-- | POST /admin/api/articles — create new article
createArticle :: AdminEnv -> ActionM ()
createArticle _env = do
  req <- jsonData :: ActionM ArticleSaveReq
  let path = asrPath req
  exists <- liftIO $ doesFileExist path
  when exists $ do
    status status409
    text "Article already exists"
    finish
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ TIO.writeFile path (asrRawContent req)
  text "Created"

-- | PUT /admin/api/articles — update existing article
updateArticle :: AdminEnv -> ActionM ()
updateArticle _env = do
  req <- jsonData :: ActionM ArticleSaveReq
  let path = asrPath req
      dir  = takeDirectory path
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ TIO.writeFile path (asrRawContent req)
  text "Updated"

-- | DELETE /admin/api/articles?path=:path — soft delete (move to .trash/)
deleteArticle :: AdminEnv -> ActionM ()
deleteArticle env = do
  path <- queryParam "path"
  exists <- liftIO $ doesFileExist path
  unless exists $ do
    status status404
    text "Article not found"
    finish
  let root     = articleDir (envConfig env)
      trashDir = root </> ".trash"
  liftIO $ createDirectoryIfMissing True trashDir
  -- Compute a trash destination preserving the relative path
  let relPath = drop (length root) path
      trashPath = trashDir </> relPath
  liftIO $ createDirectoryIfMissing True (takeDirectory trashPath)
  liftIO $ renameFile path trashPath
  text "Deleted (moved to .trash)"
