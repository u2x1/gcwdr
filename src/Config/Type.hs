module Config.Type where

import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Toml.Schema                    ( FromValue(..)
                                                , parseTableFromValue
                                                , reqKey
                                                , optKey
                                                )

data Config = Config
  { siteTitle          :: Text
  , siteUrl            :: Text
  , siteMenus          :: [Menu]
  , outputDir          :: FilePath
  , themeDir           :: FilePath
  , articleDir         :: FilePath
  , localServerPort    :: Int
  , indexes            :: [IndexConfig]
  , adminPasswordHash  :: Maybe Text
  , adminPasswordSalt  :: Maybe Text
  , adminSessionTTL    :: Maybe Int     -- seconds, default 86400 (24h)
  , adminPort          :: Maybe Int     -- default 4001
  }
  deriving Show

data IndexConfig = IndexConfig
  { idxSourceDir  :: Text
  , idxTemplate   :: Text
  , idxOutput     :: Text
  , idxSitemap    :: Bool
  } deriving Show

data Menu = Menu
  { menuName :: Text
  , menuLoc  :: Text
  }
  deriving Show

data AdminConfig = AdminConfig
  { acPasswordHash :: Maybe Text
  , acPasswordSalt :: Maybe Text
  , acSessionTTL   :: Maybe Int
  , acPort         :: Maybe Int
  } deriving Show

instance FromValue IndexConfig where
  fromValue = parseTableFromValue $ do
    src  <- reqKey "sourceDir"
    tpl  <- reqKey "template"
    out  <- reqKey "output"
    smap <- optKey "sitemap"
    pure $ IndexConfig src tpl out (fromMaybe True smap)

instance FromValue AdminConfig where
  fromValue = parseTableFromValue $
    AdminConfig
      <$> optKey "password_hash"
      <*> optKey "password_salt"
      <*> optKey "session_ttl"
      <*> optKey "port"

instance FromValue Config where
  fromValue = parseTableFromValue $ do
    title'      <- reqKey "title"
    url'        <- reqKey "url"
    menu'       <- reqKey "menu"
    outDir'     <- T.unpack <$> reqKey "outputDir"
    thmDir'     <- T.unpack <$> reqKey "themeDir"
    artDir'     <- T.unpack <$> reqKey "articleDir"
    srvPort'    <- reqKey "localServerPort"
    idxs'       <- optKey "index"
    adminCfg    <- optKey "admin"
    pure $ Config
      title' url' menu' outDir' thmDir' artDir' srvPort' (fromMaybe [] idxs')
      (adminCfg >>= acPasswordHash)
      (adminCfg >>= acPasswordSalt)
      (adminCfg >>= acSessionTTL)
      (adminCfg >>= acPort)

instance FromValue Menu where
  fromValue = parseTableFromValue $
    Menu
      <$> reqKey "name"
      <*> reqKey "loc"
