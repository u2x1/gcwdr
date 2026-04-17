module Site.FileUtils where

import           Control.Exception              ( catchJust )
import           Control.Monad                  ( filterM
                                                , guard
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.List.Extra                ( isPrefixOf
                                                , dropWhileEnd
                                                , takeWhileEnd
                                                )
import           Data.Map.Lazy                 as M
                                                ( Map
                                                , fromList
                                                , singleton
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                  as T
                                                ( readFile )
import           System.Directory               ( copyFile
                                                , createDirectory
                                                , createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , getDirectoryContents
                                                , listDirectory
                                                , removeDirectoryRecursive
                                                , removeFile
                                                )
import           System.IO.Error                ( isDoesNotExistError )
import           System.FilePath                ( (</>) )

import           Config.Parse                   ( decodeConfigFile )
import           Config.ToObjectTree            ()  -- orphan instances for Config/Menu
import           Config.Type                    ( Config )
import           Template.Type                  ( ObjectTree(..)
                                                , ToObjectTree(toObjectTree)
                                                )
import           Utils.Logging                  ( LogTag(Info)
                                                , logErrAndTerminate
                                                , logWT
                                                )

getGlbRes :: FilePath -> [FilePath] -> IO (Map Text ObjectTree)
getGlbRes themePath allFiles = do
  let partialsRes =
        filter (isPrefixOf (themePath </> "layout/partial")) allFiles
  partials <- M.singleton "partials" . ObjNode <$> getPartials partialsRes
  config   <- M.singleton "config" . toObjectTree <$> parseConfig "config.toml"
  pure $ M.singleton "global" $ ObjNode (config <> partials)
 where
  getPartials ps = do
    rawPartials <- traverse T.readFile ps
    pure $ fromList $ zip (fmap (T.pack . takeWhileEnd (/= '/')) ps)
                          (ObjLeaf <$> rawPartials)

parseConfig :: FilePath -> IO Config
parseConfig path = do
  tomlRes <- decodeConfigFile path
  case tomlRes of
    Left  errs   -> logErrAndTerminate "Parsing config" errs
    Right config -> pure config

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles root' = do
  -- Prevent it from walking into ".git", ".stack-work" dirs
  contents <- filter (not . isPrefixOf ".") <$> listDirectory root'
  let root         = if null root' || last root' == '/' then root' else root' <> "/"
      filesAndDirs = (root </>) <$> contents

  dirs     <- filterM doesDirectoryExist filesAndDirs
  subFiles <- if null dirs
    then pure []
    else mconcat <$> traverse getAllFiles dirs

  let files = filter (not . (`elem` dirs)) filesAndDirs
  return (files <> subFiles)

-- Remove anything except filename started with "." like ".git"
removeDirContent :: FilePath -> IO ()
removeDirContent root = do
  logWT Info $ "removing dir content " <> root
  ext <- doesDirectoryExist root
  if ext
    then do
      contents <- filter (not . isPrefixOf ".") <$> getDirectoryContents root
      let filesAndDirs = (root </>) <$> contents
      dirs <- filterM doesDirectoryExist filesAndDirs
      let files = filter (not . (`elem` dirs)) filesAndDirs
      traverse_ removeDirectoryRecursive dirs
      traverse_ removeFile               files
    else createDirectory root >> pure ()

copyFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFiles outputPath inputPath = traverse_
  (\source -> copyFile' source (getTarget source))
 where
  copyFile' source target = do
    logWT Info $ "copying file " <> source <> " to " <> target
    catchJust (guard . isDoesNotExistError)
              (copyFile source target)
              (\_ -> cr8Dir target >> copyFile source target)
  getTarget = (outputPath </>) . drop (length inputPath)
  cr8Dir path = createDirectoryIfMissing True (dropWhileEnd (/= '/') path)
