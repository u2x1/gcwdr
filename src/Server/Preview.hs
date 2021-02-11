{-# LANGUAGE  OverloadedStrings #-}
module Server.Preview where

import Web.Scotty
    ( file, get, param, regex, scotty, setHeader, text, ScottyM )
import System.Directory ( doesFileExist )
import qualified Data.Text.Lazy as TL
import System.FilePath ( (</>) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

previewServer :: FilePath -> Int -> IO ()
previewServer root port = scotty port $ preview root

preview :: FilePath -> ScottyM ()
preview root =
    get (regex ".*") $ do
      path <- (root <>) <$> param "0"
      filename' <- liftIO $ checkFileExist path
      case filename' of
          Nothing -> text "404"
          Just filename -> do
              setHeader "Content-Type" (getFileType filename)
              setHeader "Access-Control-Allow-Origin" "*"
              file filename


checkFileExist :: FilePath -> IO (Maybe FilePath)
checkFileExist path = do
    ifNotExist path $
        ifNotExist (path </> "index.html") $
            pure Nothing
    where
      ifNotExist path' f = do
          exist <- doesFileExist path'
          if exist
              then pure $ Just path'
              else f

getFileType :: String -> TL.Text
getFileType path = case takeWhileEnd (/= '.') path of
  "html" -> "text/html"
  "css"  -> "text/css"
  "jpg"  -> "image/jpeg"
  "png"  -> "image/png"
  "gif"  -> "image/gif"
  "tiff" -> "image/tiff"
  "js"   -> "application/javascript"
  _      -> "text/plain"
 where
  takeWhileEnd f xs = reverse $ takeWhile f (reverse xs)
