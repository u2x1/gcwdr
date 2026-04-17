module Server.Admin.Handlers.Pages where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.IORef                 ( readIORef )
import qualified Data.Map.Lazy             as M
import           Data.Text                  ( Text )
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL

import           Network.HTTP.Types.Status  ( status500 )
import           Web.Scotty                 ( ActionM, html, text, status
                                            , queryParam, queryParamMaybe, setHeader )

import           Config.Type                 ( Config(..) )
import           Template.Type               ( ObjectTree(..) )
import           Server.Admin.Types
import           Server.Admin.Templates     ( renderAdminTemplate )
import           Server.Admin.Handlers.Articles ( findMdFiles )

-- | Helper: render admin template and send as HTML
renderPage :: AdminEnv -> Text -> M.Map Text ObjectTree -> ActionM ()
renderPage env templateName vars = do
  result <- liftIO $ renderAdminTemplate (envConfig env) templateName vars
  case result of
    Right htmlContent -> do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ TL.fromStrict htmlContent
    Left errs -> do
      status status500
      text $ TL.fromStrict $ "Template error: " <> T.pack errs

-- | GET /admin/login
loginPage :: AdminEnv -> ActionM ()
loginPage env = do
  mErr <- queryParamMaybe "error" :: ActionM (Maybe Text)
  let vars = case mErr of
        Just _ -> M.singleton "login_error" (ObjLeaf "Invalid password")
        Nothing -> M.empty
  renderPage env "login" vars

-- | GET /admin/
dashboardPage :: AdminEnv -> ActionM ()
dashboardPage env = do
  let cfg = envConfig env
  -- Count articles
  articles <- liftIO $ findMdFiles (articleDir cfg)
  let articleCount = T.pack $ show $ length articles
  -- Get build status
  buildSt <- liftIO $ readIORef (envBuildState env)
  let buildStatusText = case buildSt of
        BuildIdle             -> "No builds yet"
        BuildRunning _        -> "Build running..."
        BuildFinished _ s msg -> if s then "Last build: Success" else "Last build: Failed - " <> msg
  renderPage env "dashboard" $ M.fromList
    [ ("article_count", ObjLeaf articleCount)
    , ("build_status", ObjLeaf buildStatusText)
    , ("site_title", ObjLeaf $ siteTitle cfg)
    ]

-- | GET /admin/articles
articleListPage :: AdminEnv -> ActionM ()
articleListPage env = do
  renderPage env "article-list" M.empty

-- | GET /admin/articles/new
articleNewPage :: AdminEnv -> ActionM ()
articleNewPage env = do
  let cfg = envConfig env
  renderPage env "article-editor" $ M.fromList
    [ ("editor_mode", ObjLeaf "new")
    , ("article_path", ObjLeaf "")
    , ("article_content", ObjLeaf "")
    , ("article_dir", ObjLeaf $ T.pack $ articleDir cfg)
    ]

-- | GET /admin/articles/edit?path=:path
articleEditorPage :: AdminEnv -> ActionM ()
articleEditorPage env = do
  path <- queryParam "path" :: ActionM Text
  let cfg = envConfig env
  content <- liftIO $ TIO.readFile (T.unpack path)
  renderPage env "article-editor" $ M.fromList
    [ ("editor_mode", ObjLeaf "edit")
    , ("article_path", ObjLeaf path)
    , ("article_content", ObjLeaf content)
    , ("article_dir", ObjLeaf $ T.pack $ articleDir cfg)
    ]

-- | GET /admin/build
buildPage :: AdminEnv -> ActionM ()
buildPage env = do
  renderPage env "build" M.empty

-- | GET /admin/media
mediaPage :: AdminEnv -> ActionM ()
mediaPage env = do
  renderPage env "media" M.empty

-- | GET /admin/categories
categoriesPage :: AdminEnv -> ActionM ()
categoriesPage env = do
  renderPage env "categories" M.empty
