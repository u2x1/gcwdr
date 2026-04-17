module Server.Admin.Handlers.Preview where

import           Control.Exception          ( try, SomeException )
import           Control.Monad.IO.Class     ( liftIO )
import           Data.IORef                 ( readIORef, writeIORef )
import qualified Data.Map.Lazy             as M
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL
import           System.Directory           ( getTemporaryDirectory )
import           System.FilePath            ( (</>) )

import           Network.HTTP.Types.Status  ( status500 )
import           Web.Scotty                 ( ActionM, jsonData, html, text
                                            , status, setHeader, finish )

import           Config.Type                ( Config(..) )
import           Article.Parse              ( parsePost )
import           Template.Render            ( convertTP )
import           Article.Transform          ( addGlb )
import           Site.FileUtils             ( getGlbRes, getAllFiles )
import           Site.Generate              ( getLayoutFile )
import           Server.Admin.Types

-- | POST /admin/api/preview — full themed HTML preview
previewArticle :: AdminEnv -> ActionM ()
previewArticle env = do
  req <- jsonData :: ActionM PreviewReq
  let cfg = envConfig env
      themePath = themeDir cfg

  result <- liftIO $ try $ do
    -- Ensure global resources are cached
    cached <- readIORef (envGlobalRes env)
    glbRes <- if M.null cached
      then do
        themeFiles <- getAllFiles themePath
        res <- getGlbRes themePath themeFiles
        writeIORef (envGlobalRes env) res
        pure res
      else pure cached

    -- Write content to a temp file, then parse it
    tmpDir <- getTemporaryDirectory
    let tmpFile = tmpDir </> "gcwdr-preview.md"
    TIO.writeFile tmpFile (prRawContent req)

    mObj <- parsePost tmpFile
    case mObj of
      Left err -> pure $ Left $ "Failed to parse article: " <> T.pack err
      Right obj -> do
        let fullTree = addGlb glbRes obj
            -- getLayoutFile expects the full tree (with "this" wrapper)
            layoutFile = getLayoutFile themePath fullTree
        template <- TIO.readFile layoutFile
        case convertTP fullTree template of
          Right htmlContent -> pure $ Right htmlContent
          Left errs -> pure $ Left $ "Template error: " <> T.pack errs

  case result :: Either SomeException (Either T.Text T.Text) of
    Left ex -> do
      status status500
      text $ TL.fromStrict $ "Preview error: " <> T.pack (show ex)
      finish
    Right (Left errMsg) -> do
      status status500
      text $ TL.fromStrict errMsg
      finish
    Right (Right htmlContent) -> do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ TL.fromStrict htmlContent
