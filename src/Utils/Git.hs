module Utils.Git where

import           System.Process                 ( callCommand )

deploy :: FilePath -> IO ()
deploy output = callCommand $ "cd " <> output <> " && git push"

commit :: String -> FilePath -> IO ()
commit msg output = callCommand
  ("cd " <> output <> " && git add . && git commit -m \'" <> msg <> "\'")
