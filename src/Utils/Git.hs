module Utils.Git where

import           System.Process     (callCommand)

deploy :: IO ()
deploy = callCommand "cd public && git push"

commit :: String -> IO ()
commit msg = callCommand ("cd public && git add . && git commit -m \'" <> msg <> "\'")
