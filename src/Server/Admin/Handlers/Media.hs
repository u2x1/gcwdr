module Server.Admin.Handlers.Media where

import           Control.Monad              ( unless, filterM )
import           Control.Monad.IO.Class     ( liftIO )
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL

import           Network.HTTP.Types.Status  ( status400, status404 )
import           Network.Wai.Parse          ( FileInfo(..) )
import           Web.Scotty                 ( ActionM, json, text, files
                                            , queryParamMaybe, queryParam
                                            , status, finish, formParam )

import           System.Directory           ( listDirectory, doesFileExist
                                            , doesDirectoryExist, removeFile
                                            , createDirectoryIfMissing
                                            , canonicalizePath, getFileSize )
import           System.FilePath            ( (</>) , takeFileName, takeExtension )

import           Config.Type                 ( Config(..) )
import           Server.Admin.Types

-- | Static file root = articleDir/static
staticRoot :: Config -> FilePath
staticRoot cfg = articleDir cfg </> "static"

-- | Security check: ensure path is within the static root
checkSafePath :: FilePath -> FilePath -> IO (Maybe FilePath)
checkSafePath root relPath = do
  createDirectoryIfMissing True root
  let target = root </> relPath
  canon <- canonicalizePath target
  canonRoot <- canonicalizePath root
  if canonRoot `isPrefixOfPath` canon
    then pure (Just canon)
    else pure Nothing
  where
    isPrefixOfPath prefix path = prefix == take (length prefix) path

-- | GET /admin/api/media — list files in a subdirectory
listMedia :: AdminEnv -> ActionM ()
listMedia env = do
  let root = staticRoot (envConfig env)
  mDir <- queryParamMaybe "dir" :: ActionM (Maybe T.Text)
  let subdir = maybe "" T.unpack mDir
  safePath <- liftIO $ checkSafePath root subdir
  case safePath of
    Nothing -> do
      status status400
      text "Invalid directory"
      finish
    Just dirPath -> do
      exists <- liftIO $ doesDirectoryExist dirPath
      unless exists $ do
        json ([] :: [MediaFile])
        finish
      entries <- liftIO $ listDirectory dirPath
      let entryPaths = map (dirPath </>) entries
      filePaths <- liftIO $ filterM doesFileExist entryPaths
      mediaFiles <- liftIO $ mapM (toMediaFile root) filePaths
      json mediaFiles

-- | Convert a file path to MediaFile
toMediaFile :: FilePath -> FilePath -> IO MediaFile
toMediaFile root fp = do
  size <- getFileSize fp
  canonRoot <- canonicalizePath root
  let relPath = drop (length canonRoot + 1) fp
      name = T.pack $ takeFileName fp
      ext  = map toLowerChar $ takeExtension fp
      mime = extToMime ext
      isImg = ext `elem` [".jpg", ".jpeg", ".png", ".gif", ".webp", ".svg", ".bmp", ".tiff"]
  pure $ MediaFile name (T.pack relPath) (fromIntegral size) (T.pack mime) isImg
  where
    toLowerChar c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- | Extension to MIME type
extToMime :: String -> String
extToMime ".jpg"  = "image/jpeg"
extToMime ".jpeg" = "image/jpeg"
extToMime ".png"  = "image/png"
extToMime ".gif"  = "image/gif"
extToMime ".webp" = "image/webp"
extToMime ".svg"  = "image/svg+xml"
extToMime ".bmp"  = "image/bmp"
extToMime ".tiff" = "image/tiff"
extToMime ".pdf"  = "application/pdf"
extToMime ".js"   = "application/javascript"
extToMime ".css"  = "text/css"
extToMime ".html" = "text/html"
extToMime ".txt"  = "text/plain"
extToMime ".mp4"  = "video/mp4"
extToMime ".mp3"  = "audio/mpeg"
extToMime _       = "application/octet-stream"

-- | POST /admin/api/media/upload — multipart file upload
uploadMedia :: AdminEnv -> ActionM ()
uploadMedia env = do
  let root = staticRoot (envConfig env)
  mDir <- queryParamMaybe "dir" :: ActionM (Maybe T.Text)
  let subdir = maybe "uploads" T.unpack mDir
  safePath <- liftIO $ checkSafePath root subdir
  case safePath of
    Nothing -> do
      status status400
      text "Invalid upload directory"
      finish
    Just dirPath -> do
      liftIO $ createDirectoryIfMissing True dirPath
      uploaded <- files
      case uploaded of
        [] -> do
          status status400
          text "No file uploaded"
          finish
        ((_, finfo):_) -> do
          let fname = bsToFilename (fileName finfo)
              dest  = dirPath </> takeFileName fname
          liftIO $ BL.writeFile dest (fileContent finfo)
          text $ TL.fromStrict $ "Uploaded: " <> T.pack (takeFileName fname)

-- | Convert ByteString filename to String, handling UTF-8
bsToFilename :: BS.ByteString -> FilePath
bsToFilename bs = case TE.decodeUtf8' bs of
  Right t -> T.unpack t
  Left _  -> map (toEnum . fromEnum) (BS.unpack bs)

-- | DELETE /admin/api/media?path= — delete a file
deleteMedia :: AdminEnv -> ActionM ()
deleteMedia env = do
  let root = staticRoot (envConfig env)
  relPath <- queryParam "path" :: ActionM T.Text
  safePath <- liftIO $ checkSafePath root (T.unpack relPath)
  case safePath of
    Nothing -> do
      status status400
      text "Invalid path"
      finish
    Just filePath -> do
      exists <- liftIO $ doesFileExist filePath
      unless exists $ do
        status status404
        text "File not found"
        finish
      liftIO $ removeFile filePath
      text "Deleted"

-- | POST /admin/api/media/mkdir — create subdirectory
mkdirMedia :: AdminEnv -> ActionM ()
mkdirMedia env = do
  let root = staticRoot (envConfig env)
  dirName <- formParam "dir" :: ActionM T.Text
  safePath <- liftIO $ checkSafePath root (T.unpack dirName)
  case safePath of
    Nothing -> do
      status status400
      text "Invalid directory name"
      finish
    Just dirPath -> do
      liftIO $ createDirectoryIfMissing True dirPath
      text $ TL.fromStrict $ "Created: " <> dirName
