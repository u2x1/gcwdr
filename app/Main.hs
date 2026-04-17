module Main where

import           Config.Parse                   ( decodeConfigFile )
import           Config.Type                    ( Config
                                                  ( articleDir
                                                  , localServerPort
                                                  , outputDir
                                                  , themeDir
                                                  )
                                                )
import           Site.Generate                  ( gnrtPublic )
import           Utils.Git                      ( commit
                                                , deploy
                                                )
import           Server.Preview                 ( previewServer )
import           Server.Admin.Server            ( adminServer )
import           Server.Admin.Setup             ( adminSetup )

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           System.FSNotify                ( watchTree
                                                , withManager
                                                )
import           Utils.Logging                  ( LogTag(Info)
                                                , logErrAndTerminate
                                                , logWT
                                                )

import           Options.Applicative

data Options = Options
  { optPreGenerate :: Bool
  , optWatch       :: Bool
  , optCommand     :: Command
  }

data Command
  = Generate
  | Server
  | Commit String
  | Deploy
  | Admin
  | AdminSetup

optionsParser :: ParserInfo Options
optionsParser = info (optParser <**> helper)
  (  fullDesc
  <> header "gcwdr - a static site generator"
  )

optParser :: Parser Options
optParser = Options
  <$> switch
      (  short 'g'
      <> help "Pre-generate output files"
      )
  <*> switch
      (  short 'w'
      <> help "Watch file changes and regenerate"
      )
  <*> commandParser

commandParser :: Parser Command
commandParser = subparser
  (  command "generate"
       (info (pure Generate)
             (progDesc "Generate static html files"))
  <> command "server"
       (info (pure Server)
             (progDesc "Start local preview server"))
  <> command "commit"
       (info commitParser
             (progDesc "Commit to git"))
  <> command "deploy"
       (info (pure Deploy)
             (progDesc "Push the static html files to remote"))
  <> command "admin"
       (info (pure Admin)
             (progDesc "Start admin server"))
  <> command "admin-setup"
       (info (pure AdminSetup)
             (progDesc "Set up admin password"))
  )

commitParser :: Parser Command
commitParser = Commit
  <$> argument str (metavar "MESSAGE" <> help "Commit message")

parseConfig :: FilePath -> IO Config
parseConfig path = do
  tomlRes <- decodeConfigFile path
  case tomlRes of
    Left  errs   -> logErrAndTerminate "Parsing config" errs
    Right config -> pure config

main :: IO ()
main = do
  opts <- execParser optionsParser
  config <- parseConfig "config.toml"

  when (optPreGenerate opts) $ gnrtPublic config
  when (optWatch opts)       $ () <$ forkIO (watchChanges config)

  case optCommand opts of
    Generate   -> gnrtPublic config
    Server     -> previewServer (outputDir config) (localServerPort config)
    Commit msg -> commit msg (outputDir config)
    Deploy     -> deploy (outputDir config)
    Admin      -> adminServer config
    AdminSetup -> adminSetup config

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
