module Server.Admin.Setup where

import           Data.Text                  ( Text )
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Time                  ( getCurrentTime )
import           System.IO                  ( hFlush, stdout, hSetEcho, stdin )

import           Data.Config.Type           ( Config(..) )
import           Server.Admin.Auth          ( hashPassword, toHex, sha256 )
import qualified Data.Text.Encoding        as TE
import           Utils.Logging              ( LogTag(Info), logWT )

-- | Interactive admin password setup
adminSetup :: Config -> IO ()
adminSetup _cfg = do
  logWT Info "admin-setup: configuring admin password"

  putStr "Enter admin password: "
  hFlush stdout
  hSetEcho stdin False
  password <- getLine
  putStrLn ""

  putStr "Confirm admin password: "
  hFlush stdout
  password2 <- getLine
  hSetEcho stdin True
  putStrLn ""

  if password /= password2
    then putStrLn "Error: passwords do not match."
    else do
      -- Generate salt from current time
      now <- getCurrentTime
      let salt = toHex $ sha256 $ TE.encodeUtf8 $ T.pack $ show now
          passwordText = T.pack password
          hash = hashPassword salt passwordText

      -- Read current config and append/update [admin] section
      configContent <- TIO.readFile "config.toml"
      let newContent = updateAdminSection configContent salt hash
      TIO.writeFile "config.toml" newContent

      logWT Info "admin-setup: password configured successfully"
      putStrLn "Admin password has been set. Start the admin server with: gcwdr admin"

-- | Update or append the [admin] section in config.toml
updateAdminSection :: Text -> Text -> Text -> Text
updateAdminSection content salt hash =
  let ls = T.lines content
      -- Check if [admin] section exists
      hasAdmin = any (T.isPrefixOf "[admin]" . T.strip) ls
  in if hasAdmin
     then T.unlines $ updateExistingAdmin ls salt hash
     else content <> "\n" <> adminSection salt hash

-- | Replace password_hash and password_salt in existing [admin] section
updateExistingAdmin :: [Text] -> Text -> Text -> [Text]
updateExistingAdmin [] _ _ = []
updateExistingAdmin (l:ls) salt hash
  | T.isPrefixOf "password_hash" (T.strip l) =
      ("password_hash = \"" <> hash <> "\"") : updateExistingAdmin ls salt hash
  | T.isPrefixOf "password_salt" (T.strip l) =
      ("password_salt = \"" <> salt <> "\"") : updateExistingAdmin ls salt hash
  | T.isPrefixOf "[admin]" (T.strip l) =
      l : addFieldsIfMissing ls salt hash
  | otherwise = l : updateExistingAdmin ls salt hash
  where
    addFieldsIfMissing rest s h =
      let hasHash = any (T.isPrefixOf "password_hash" . T.strip) rest
          hasSalt = any (T.isPrefixOf "password_salt" . T.strip) rest
          newLines = (if hasHash then [] else ["password_hash = \"" <> h <> "\""]) ++
                     (if hasSalt then [] else ["password_salt = \"" <> s <> "\""])
      in newLines ++ updateExistingAdmin rest s h

-- | Generate a new [admin] section
adminSection :: Text -> Text -> Text
adminSection salt hash = T.unlines
  [ "[admin]"
  , "port = 4001"
  , "session_ttl = 86400"
  , "password_hash = \"" <> hash <> "\""
  , "password_salt = \"" <> salt <> "\""
  ]
