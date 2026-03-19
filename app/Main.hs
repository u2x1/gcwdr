module Main where

import           Data.Config                    ( decodeConfigFile )
import           Data.Config.Type               ( Config
                                                  ( articleDir
                                                  , localServerPort
                                                  , outputDir
                                                  , themeDir
                                                  , adminPasswordHash
                                                  , adminPasswordSalt
                                                  )
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Maybe                     ( mapMaybe )
import           Entry.Read                     ( gnrtPublic )
import           System.Environment             ( getArgs )
import           Utils.Git                      ( commit
                                                , deploy
                                                )
import           Server.Preview                 ( previewServer )
import           Server.Admin.Server            ( adminServer )
import           Server.Admin.Setup             ( adminSetup )

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever )
import           System.FSNotify                ( watchTree
                                                , withManager
                                                )
import           Utils.Logging                  ( LogTag(Info)
                                                , logErrAndTerminate
                                                , logWT
                                                )

parseConfig :: FilePath -> IO Config
parseConfig path = do
  tomlRes <- decodeConfigFile path
  case tomlRes of
    Left  errs   -> logErrAndTerminate "Parsing config" errs
    Right config -> pure config

test :: IO ()
test = do
  config <- Main.parseConfig "config.toml"
  gnrtPublic config
  previewServer "test-data/public" 4000

main :: IO ()
main = do
  args <- getArgs
  let cmds = parseArgs args

  config <- parseConfig "config.toml"

  let runFlag x = case x of
        PreGenerate -> gnrtPublic config
        Watch       -> () <$ forkIO (watchChanges config)

  traverse_ runFlag (takeFlags cmds)


  case cmds of
    Right (Command Generate _) -> gnrtPublic config
    Right (Command Server _) ->
      previewServer (outputDir config) (localServerPort config)
    Right (Command (Commit msg) _) -> commit msg (outputDir config)
    Right (Command Deploy       _) -> deploy (outputDir config)
    Right (Command Admin        _) -> adminServer config
    Right (Command AdminSetup   _) -> adminSetup config
    Right (Command Help         _) -> putStrLn usage

    Left err -> putStrLn ("Error: " <> err) >> putStrLn usage
 where
  takeFlags (Right (Command _ xs)) = xs
  takeFlags _                      = []

usage :: String
usage =
  "usage: ./gcwdr <command> \n\
        \commands:\n\
        \  generate\t\t\tGenerate static html files.\n\
        \  server\t\t\tStart local server with a simple TCP socket\n\
        \  commit <Message>\tCommit to git.\n\
        \  help\t\t\tShow this help text.\n\
        \  deploy\t\t\tPush the static html files to Github.\n\
        \  admin\t\t\tStart admin server on port 4001.\n\
        \  admin-setup\t\tSet up admin password.\n\
        \available options:\n\
        \  -g: generate output files\n\
        \  -w: watch file changes"

data Command = Command Mode [Flag]
data Mode = Generate | Server | Commit String | Deploy | Help | Admin | AdminSetup
data Flag = PreGenerate | Watch

parseArgs :: [String] -> Either String Command
parseArgs args =
  Command <$> parseMode (filter (not . isFlag) args) <*> Right
    (parseFlag $ filter isFlag args)
 where
  isFlag ('-':_) = True
  isFlag _       = False

  parseMode []       = Left "Empty command."
  parseMode (x : xs) = case x of
    "generate"    -> Right Generate
    "server"      -> Right Server
    "deploy"      -> Right Deploy
    "help"        -> Right Help
    "admin"       -> Right Admin
    "admin-setup" -> Right AdminSetup
    "commit"      -> case xs of
      []      -> Left "Commit message can not be empty."
      (m : _) -> Right $ Commit m
    _ -> Left "Unknown command."

  parseFlag = mapMaybe getFlag
  getFlag ('-':rest) = case rest of
    "w" -> Just Watch
    "g" -> Just PreGenerate
    _   -> Nothing
  getFlag _ = Nothing

watchChanges :: Config -> IO a
watchChanges cfg =
  let ctDir = articleDir cfg
      tmDir = themeDir cfg
  in  withManager $ \man -> do
        let forkWatch dir = watchTree man dir (const True) recvChange
            recvChange = const $ do
              logWT Info "source directory polluted, regenerating sites"
              () <$ forkIO (gnrtPublic cfg)

        _ <- forkWatch ctDir
        _ <- forkWatch tmDir
        forever $ threadDelay 1000000
