module Server.Admin.Server where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.IORef                 ( newIORef )
import qualified Data.Map.Lazy             as M
import           Data.Maybe                 ( fromMaybe )
import qualified Data.Text                 as T

import           Web.Scotty                 ( scotty, get, post, put, delete
                                            , text, redirect, formParam
                                            , finish, file, setHeader
                                            , regex, captureParam
                                            , ScottyM, ActionM )

import           System.FilePath            ( (</>) )

import           Data.Config.Type           ( Config(..) )
import           Server.Admin.Auth
import           Server.Admin.Types

import           Server.Admin.Handlers.Articles ( listArticles, getArticle
                                                , createArticle, updateArticle
                                                , deleteArticle )
import           Server.Admin.Handlers.Preview  ( previewArticle )
import           Server.Admin.Handlers.Build    ( triggerBuild, buildStatus )
import           Server.Admin.Handlers.Pages    ( loginPage, dashboardPage
                                                , articleListPage, articleEditorPage
                                                , articleNewPage, buildPage
                                                , mediaPage, categoriesPage )
import           Server.Admin.Handlers.Media      ( listMedia, uploadMedia
                                                  , deleteMedia, mkdirMedia )
import           Server.Admin.Handlers.Categories ( listCategories )
import           Server.Preview               ( checkFileExist, getFileType )

import           Utils.Logging              ( LogTag(Info), logWT )

-- | Start the admin server on the configured port (default 4001)
adminServer :: Config -> IO ()
adminServer cfg = do
  let port = fromMaybe 4001 (adminPort cfg)
  sessStore  <- newIORef []
  buildState <- newIORef BuildIdle
  glbRes     <- newIORef M.empty

  let env = AdminEnv cfg sessStore buildState glbRes

  logWT Info $ "admin server at http://localhost:" <> show port
  scotty port (adminRoutes env)

adminRoutes :: AdminEnv -> ScottyM ()
adminRoutes env = do
  -- ---- Page routes ----
  get  "/admin/login" $ loginPage env
  post "/admin/login" $ handleLogin env
  post "/admin/logout" $ handleLogout env

  get "/admin/"                $ do _ <- requireAuth env; dashboardPage env
  get "/admin/articles"        $ do _ <- requireAuth env; articleListPage env
  get "/admin/articles/new"    $ do _ <- requireAuth env; articleNewPage env
  get "/admin/articles/edit"   $ do _ <- requireAuth env; articleEditorPage env
  get "/admin/build"           $ do _ <- requireAuth env; buildPage env
  get "/admin/media"           $ do _ <- requireAuth env; mediaPage env
  get "/admin/categories"      $ do _ <- requireAuth env; categoriesPage env

  -- ---- API routes ----
  get    "/admin/api/articles"       $ do _ <- requireAuthAPI env; listArticles env
  get    "/admin/api/articles/get"   $ do _ <- requireAuthAPI env; getArticle env
  post   "/admin/api/articles"       $ do _ <- requireAuthAPI env; createArticle env
  put    "/admin/api/articles"       $ do _ <- requireAuthAPI env; updateArticle env
  delete "/admin/api/articles"       $ do _ <- requireAuthAPI env; deleteArticle env

  post "/admin/api/preview"          $ do _ <- requireAuthAPI env; previewArticle env
  post "/admin/api/build/trigger"    $ do _ <- requireAuthAPI env; triggerBuild env
  get  "/admin/api/build/status"     $ do _ <- requireAuthAPI env; buildStatus env

  -- Media API
  get    "/admin/api/media"          $ do _ <- requireAuthAPI env; listMedia env
  post   "/admin/api/media/upload"   $ do _ <- requireAuthAPI env; uploadMedia env
  delete "/admin/api/media"          $ do _ <- requireAuthAPI env; deleteMedia env
  post   "/admin/api/media/mkdir"    $ do _ <- requireAuthAPI env; mkdirMedia env

  -- Categories API
  get    "/admin/api/categories"     $ do _ <- requireAuthAPI env; listCategories env

  -- Static file catch-all (must be last)
  get (regex "(.*)") $ serveStatic env


-- | Handle login POST
handleLogin :: AdminEnv -> ActionM ()
handleLogin env = do
  let cfg = envConfig env
  case (adminPasswordHash cfg, adminPasswordSalt cfg) of
    (Just storedHash, Just salt) -> do
      password <- formParam "password"
      if checkPassword salt storedHash password
        then do
          token <- liftIO $ createSession (envSessionStore env)
          setSessionCookie token
          _ <- redirect "/admin/"
          finish
        else do
          _ <- redirect "/admin/login?error=1"
          finish
    _ -> do
      text "Admin password not configured. Run: gcwdr admin-setup"
      finish

-- | Handle logout POST
handleLogout :: AdminEnv -> ActionM ()
handleLogout env = do
  mToken <- getSessionToken
  case mToken of
    Just token -> liftIO $ removeSession (envSessionStore env) token
    Nothing    -> pure ()
  clearSessionCookie
  _ <- redirect "/admin/login"
  finish

-- | Serve static files from outputDir, theme/static, or article/static
serveStatic :: AdminEnv -> ActionM ()
serveStatic env = do
  let cfg = envConfig env
  path <- T.unpack <$> captureParam "1"
  let candidates = [ outputDir cfg <> path
                   , themeDir cfg </> "static" <> path
                   , articleDir cfg </> "static" <> path
                   ]
  found <- liftIO $ findFirst candidates
  case found of
    Nothing -> do
      text "404"
      finish
    Just fp -> do
      setHeader "Content-Type" (getFileType fp)
      setHeader "Access-Control-Allow-Origin" "*"
      file fp
  where
    findFirst [] = pure Nothing
    findFirst (c:cs) = do
      mf <- checkFileExist c
      case mf of
        Just f  -> pure (Just f)
        Nothing -> findFirst cs
