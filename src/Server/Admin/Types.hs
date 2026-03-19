{-# LANGUAGE DeriveGeneric #-}
module Server.Admin.Types where

import Data.Aeson            ( FromJSON, ToJSON, (.=), (.:), (.:?)
                             , object, withObject, parseJSON, toJSON )
import Data.IORef            ( IORef )
import Data.Map.Lazy         ( Map )
import Data.Text             ( Text )
import Data.Time             ( UTCTime )
import GHC.Generics          ( Generic )

import Data.Config.Type      ( Config )
import Data.Template.Type    ( ObjectTree )

-- | Session token + expiry
data Session = Session
  { sessToken   :: Text
  , sessExpires :: UTCTime
  } deriving (Show, Eq)

type SessionStore = IORef [Session]

-- | Build status state machine
data BuildStatus
  = BuildIdle
  | BuildRunning UTCTime
  | BuildFinished UTCTime Bool Text   -- time, success?, message
  deriving (Show, Eq)

type BuildState = IORef BuildStatus

-- | Article metadata (JSON output)
data ArticleMeta = ArticleMeta
  { amPath     :: FilePath
  , amTitle    :: Text
  , amDate     :: Text
  , amCategory :: Text
  , amTemplate :: Text
  } deriving (Show, Generic)

instance ToJSON ArticleMeta where
  toJSON a = object
    [ "path"     .= amPath a
    , "title"    .= amTitle a
    , "date"     .= amDate a
    , "category" .= amCategory a
    , "template" .= amTemplate a
    ]

-- | Full article (meta + raw content)
data ArticleFull = ArticleFull
  { afMeta       :: ArticleMeta
  , afRawContent :: Text
  } deriving (Show)

instance ToJSON ArticleFull where
  toJSON a = object
    [ "path"        .= amPath (afMeta a)
    , "title"       .= amTitle (afMeta a)
    , "date"        .= amDate (afMeta a)
    , "category"    .= amCategory (afMeta a)
    , "template"    .= amTemplate (afMeta a)
    , "raw_content" .= afRawContent a
    ]

-- | Save request (JSON input)
data ArticleSaveReq = ArticleSaveReq
  { asrPath       :: FilePath
  , asrRawContent :: Text
  } deriving (Show)

instance FromJSON ArticleSaveReq where
  parseJSON = withObject "ArticleSaveReq" $ \v ->
    ArticleSaveReq <$> v .: "path" <*> v .: "raw_content"

-- | Preview request (JSON input)
data PreviewReq = PreviewReq
  { prRawContent :: Text
  , prTemplate   :: Maybe Text
  } deriving (Show)

instance FromJSON PreviewReq where
  parseJSON = withObject "PreviewReq" $ \v ->
    PreviewReq <$> v .: "raw_content" <*> v .:? "template"

-- | Build status JSON response
data BuildStatusResp = BuildStatusResp
  { bsrStatus  :: Text
  , bsrSuccess :: Maybe Bool
  , bsrMessage :: Maybe Text
  } deriving (Show)

instance ToJSON BuildStatusResp where
  toJSON b = object
    [ "status"  .= bsrStatus b
    , "success" .= bsrSuccess b
    , "message" .= bsrMessage b
    ]

-- | Shared admin environment
data AdminEnv = AdminEnv
  { envConfig       :: Config
  , envSessionStore :: SessionStore
  , envBuildState   :: BuildState
  , envGlobalRes    :: IORef (Map Text ObjectTree)
  }
