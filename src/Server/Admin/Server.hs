module Server.Admin.Server where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.IORef                 ( newIORef )
import qualified Data.Map.Lazy             as M
import           Data.Maybe                 ( fromMaybe )

import           Web.Scotty                 ( scotty, get, post, put, delete
                                            , text, redirect, formParam
                                            , finish
                                            , ScottyM, ActionM )

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
                                                , articleNewPage, buildPage )

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

  -- ---- API routes ----
  get    "/admin/api/articles"       $ do _ <- requireAuthAPI env; listArticles env
  get    "/admin/api/articles/get"   $ do _ <- requireAuthAPI env; getArticle env
  post   "/admin/api/articles"       $ do _ <- requireAuthAPI env; createArticle env
  put    "/admin/api/articles"       $ do _ <- requireAuthAPI env; updateArticle env
  delete "/admin/api/articles"       $ do _ <- requireAuthAPI env; deleteArticle env

  post "/admin/api/preview"          $ do _ <- requireAuthAPI env; previewArticle env
  post "/admin/api/build/trigger"    $ do _ <- requireAuthAPI env; triggerBuild env
  get  "/admin/api/build/status"     $ do _ <- requireAuthAPI env; buildStatus env


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
