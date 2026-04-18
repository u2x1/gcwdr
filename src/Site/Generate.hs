module Site.Generate where

import           Control.Monad                  ( unless )
import           Data.Either                    ( partitionEithers )
import           Data.Foldable                  ( traverse_ )
import           Data.List.Extra                ( isPrefixOf
                                                , isSuffixOf
                                                , nub
                                                , sortOn
                                                )
import           Data.Map.Lazy                 as M
                                                ( singleton
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Ord                       ( Down(Down) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                  as T
                                                ( readFile
                                                , writeFile
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesDirectoryExist
                                                )
import           System.FilePath                ( (</>) )
import           System.FilePath                ( takeDirectory
                                                )

import           Article.Parse                  ( parsePost )
import           Article.Query                  ( getCategory
                                                , getDate
                                                , getLeaf'
                                                , getNode
                                                )
import           Article.Transform              ( addGlb
                                                , runAtclModule
                                                , toNodeList
                                                )
import           Config.Type                    ( Config
                                                  ( articleDir
                                                  , outputDir
                                                  , siteUrl
                                                  , themeDir
                                                  , indexes
                                                  )
                                                , IndexConfig(..)
                                                )
import           Template.Render                ( convertTP )
import           Template.Type                  ( ObjectTree(..) )
import           Site.FileUtils                 ( getAllFiles
                                                , copyFiles
                                                , getGlbRes
                                                , removeDirContent
                                                )
import           Site.Sitemap                   ( getSitemap )
import           Utils.Logging                  ( LogTag(Info, Warning)
                                                , logErrAndTerminate
                                                , logWT
                                                )

gnrtPublic :: Config -> IO ()
gnrtPublic cfg = do
  logWT Info "generating output"
  let atclPath  = articleDir cfg
      themePath = themeDir cfg
  articleRes <- getAllFiles atclPath
  themeRes   <- getAllFiles themePath
  let getMdRes path = (flip filter) articleRes
        (\p -> (atclPath </> T.unpack path) `isPrefixOf` p && ".md" `isSuffixOf` p)
      cStatics = filter (isPrefixOf (atclPath </> "static/")) articleRes
      statics  = filter (isPrefixOf (themePath </> "static/")) themeRes

  glbRes   <- getGlbRes themePath themeRes

  let parseObj x = do
        results <- traverse parsePost x
        let (errs, objs) = partitionEithers results
        unless (null errs) $ logWT Warning $ "parse errors:\n" <> unlines errs
        pure objs

  -- Validate that configured sourceDirs exist
  traverse_ (\idx -> do
    let dir = atclPath </> T.unpack (idxSourceDir idx)
    exist <- doesDirectoryExist dir
    unless exist $ logErrAndTerminate ("index sourceDir") (dir <> " does not exist")
    ) (indexes cfg)

  -- Parse articles from each index's sourceDir
  idxObjsList <- traverse (\idx -> do
    let srcFiles = getMdRes (idxSourceDir idx)
    objs <- sortOn (Down . getDate) <$> parseObj srcFiles
    pure (idx, objs)
    ) (indexes cfg)

  pageObjs <- parseObj (getMdRes "page/")

  -- Build index object trees for rendering
  let indexTrees = flip fmap idxObjsList $ \(idx, objs) ->
        let cates = (\x -> mconcat
                        [ M.singleton "cateName" (ObjLeaf x)
                        , M.singleton "posts" (toNodeList (filter ((== Just x) . getCategory) objs))
                        ]
                      ) <$> catMaybes (nub $ getCategory <$> objs)
            objTree = addGlb glbRes $ ObjNode $ mconcat
                        [ M.singleton "posts" (toNodeList objs)
                        , M.singleton "categories" (ObjNodeList cates)
                        ]
        in (idx, objTree)

  -- Remove out-dated public dir.
  _ <- removeDirContent (outputDir cfg)

  -- Copy static files.
  let outputPath = outputDir cfg
  copyFiles outputPath (themePath </> "static/") statics
  copyFiles outputPath (atclPath </> "static/")  cStatics

  -- Generate index pages from config
  traverse_ (\(idx, objTree) -> do
    let tplPath = themePath </> "layout" </> T.unpack (idxTemplate idx)
        outPath = outputDir cfg </> T.unpack (idxOutput idx)
    logWT Info $ "generating index " <> T.unpack (idxOutput idx) <> " from " <> T.unpack (idxTemplate idx)
    tpl <- T.readFile tplPath
    html <- getFromTP (T.unpack $ idxTemplate idx) (convertTP objTree tpl)
    createDirectoryIfMissing True (takeDirectory outPath)
    T.writeFile outPath html
    ) indexTrees

  -- Generate article pages
  let allIdxObjs = concatMap snd idxObjsList
      articles = addGlb glbRes <$> runAtclModule (allIdxObjs <> pageObjs)

  gnrtSitemap outputPath (siteUrl cfg) $
      addGlb glbRes <$> runAtclModule allIdxObjs

  gnrtHtmls outputPath themePath articles


getFromTP :: String -> Either String a -> IO a
getFromTP obj (Left  x) = logErrAndTerminate ("parsing " <> obj) x
getFromTP _   (Right x) = pure x

getLayoutFile :: FilePath -> ObjectTree -> FilePath
getLayoutFile themePath x = case getNode "this" x >>= getLeaf' "template" of
  Just t -> themePath </> "layout" </> T.unpack t <> ".html"
  _      -> themePath </> "layout/nolayout.html"

fromMaybeM :: String -> Maybe a -> IO a
fromMaybeM _ (Just x) = pure x
fromMaybeM msg _ = logErrAndTerminate msg "encountered Nothing in fromMaybe"

gnrtHtmls :: FilePath -> FilePath -> [ObjectTree] -> IO ()
gnrtHtmls outputPath themePath = traverse_
  (\x -> do
    title <- pure $ maybe "<untitled>" id
                        (T.unpack <$> (getNode "this" x >>= getLeaf' "title"))
    relLink <- fromMaybeM
      "getting relLink from article"
      ((outputPath </>) . T.unpack . T.tail <$> (getNode "this" x >>= getLeaf' "relLink")
      )
    logWT Info $ "generating article \"" <> title <> "\" at " <> relLink
    html <- getFromTP "articles" . convertTP x =<< T.readFile
      (getLayoutFile themePath x)
    _ <- createDirectoryIfMissing True relLink
    T.writeFile (relLink </> "index.html") html
  )

gnrtSitemap :: FilePath -> Text -> [ObjectTree] -> IO ()
gnrtSitemap outputPath site objs = do
  let siteMapPath = outputPath </> "sitemap.xml"
  logWT Info $ "generating sitemap at " <> siteMapPath
  sitemap <- getSitemap site objs
  T.writeFile siteMapPath sitemap
