module Server.Admin.Handlers.Build where

import           Control.Concurrent         ( forkIO )
import           Control.Exception          ( try, SomeException )
import           Control.Monad.IO.Class     ( liftIO )
import           Data.IORef                 ( readIORef, writeIORef )
import qualified Data.Text                 as T
import           Data.Time                  ( getCurrentTime )

import           Network.HTTP.Types.Status  ( status409 )
import           Web.Scotty                 ( ActionM, json, text, status, finish )

import           Site.Generate              ( gnrtPublic )
import           Server.Admin.Types
import           Utils.Logging              ( LogTag(Info), logWT )

-- | POST /admin/api/build/trigger — start site generation
triggerBuild :: AdminEnv -> ActionM ()
triggerBuild env = do
  let bsRef = envBuildState env
      cfg   = envConfig env
  current <- liftIO $ readIORef bsRef
  case current of
    BuildRunning _ -> do
      status status409
      text "Build already running"
      finish
    _ -> do
      now <- liftIO getCurrentTime
      liftIO $ writeIORef bsRef (BuildRunning now)
      liftIO $ logWT Info "admin: triggering site build"
      _ <- liftIO $ forkIO $ do
        result <- try (gnrtPublic cfg) :: IO (Either SomeException ())
        finishTime <- getCurrentTime
        case result of
          Right () -> do
            logWT Info "admin: build completed successfully"
            writeIORef bsRef (BuildFinished finishTime True "Build completed successfully")
          Left err -> do
            logWT Info $ "admin: build failed: " <> show err
            writeIORef bsRef (BuildFinished finishTime False (T.pack $ show err))
      json $ BuildStatusResp "running" Nothing Nothing

-- | GET /admin/api/build/status — poll current build status
buildStatus :: AdminEnv -> ActionM ()
buildStatus env = do
  current <- liftIO $ readIORef (envBuildState env)
  json $ case current of
    BuildIdle ->
      BuildStatusResp "idle" Nothing Nothing
    BuildRunning _ ->
      BuildStatusResp "running" Nothing Nothing
    BuildFinished _ success msg ->
      BuildStatusResp "finished" (Just success) (Just msg)
