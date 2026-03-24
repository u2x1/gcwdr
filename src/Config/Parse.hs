module Config.Parse where

import qualified Data.Text.IO                  as TIO
import           Toml                           ( decode
                                                , Result(..)
                                                )

import           Config.Type                    ( Config(..) )

decodeConfigFile :: FilePath -> IO (Either String Config)
decodeConfigFile path = do
  content <- TIO.readFile path
  case decode content of
    Failure errs -> pure $ Left (unlines errs)
    Success _warnings val -> pure $ Right val
