{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment        (getArgs)
import           Entry.Read                (gnrtPublic)
import           Utils.Server              (runBlogPreview)
import           Utils.Git                 (deploy, commit)
import           Data.Maybe                (catMaybes)
import           Data.Foldable             (traverse_)

main :: IO ()
main = do
  let root = "."
  args <- getArgs
  let cmds = parseArgs args

  let runFlag x = case x of
                    PreGenerate -> gnrtPublic root

  traverse_ runFlag (takeFlags cmds)

  case cmds of
    Right (Command Generate     _) -> gnrtPublic root
    Right (Command Server       _) -> runBlogPreview (root <> "/public/")
    Right (Command (Commit msg) _) -> commit msg
    Right (Command Deploy       _) -> deploy
    Right (Command Help         _) -> putStrLn usage

    Left err -> putStrLn ("Error: " <> err) >> putStrLn usage
  where
    takeFlags (Right (Command _ xs)) = xs
    takeFlags _ = []

usage :: String
usage = "usage: ./gcwdr <command>\n\
        \commands:\n\
        \  generate(g):\tGenerate static html files.\n\
        \  server(s):\tStart local server with a simple TCP socket at http://localhost:4000.\n\
        \  commit(c) <Message>:\tCommit to git.\n\
        \  help(h):\tShow this help text.\n\
        \  deploy(d):\tPush the static html files to Github."

data Command = Command Mode [Flag]
data Mode = Generate | Server | Commit String | Deploy | Help
data Flag = PreGenerate

parseArgs :: [String] -> Either String Command
parseArgs args = Command <$> (parseMode $ filter ((/= '-').head) args) <*> (Right $ parseFlag $ filter ((== '-').head) args)
  where
    parseMode [] = Left "Empty command."
    parseMode (x:xs) = case x of
                          "generate" -> Right $ Generate
                          "server"   -> Right $ Server
                          "deploy"   -> Right $ Deploy
                          "commit"   -> if null xs then Left "Commit message can not be empty." else Right $ Commit (head xs)
                          "help"     -> Right $ Help
                          "h"        -> Right $ Help
                          _          -> Left "Unknown command."

    parseFlag xs = catMaybes $ map getFlag xs
    getFlag x = case tail x of
                  "g" -> Just PreGenerate
                  _   -> Nothing
