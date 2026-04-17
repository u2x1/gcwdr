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
import           System.Directory               ( createDirectoryIfMissing )
import           System.FilePath                ( (</>) )

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
                                                  )
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
        (\p -> (atclPath </> path) `isPrefixOf` p && ".md" `isSuffixOf` p)

  let posts = getMdRes "post/"
      pages = getMdRes "page/"
      diary = getMdRes "diary/"
      writeup = getMdRes "writeup/"
      cStatics = filter (isPrefixOf (atclPath </> "static/")) articleRes
      statics  = filter (isPrefixOf (themePath </> "static/")) themeRes

  glbRes   <- getGlbRes themePath themeRes

  let parseObj x = do
        results <- traverse parsePost x
        let (errs, objs) = partitionEithers results
        unless (null errs) $ logWT Warning $ "parse errors:\n" <> unlines errs
        pure objs
  postObjs <- sortOn (Down . getDate) <$> parseObj posts
  diaryObjs<- sortOn (Down . getDate) <$> parseObj diary
  writeupObjs <- sortOn (Down . getDate) <$> parseObj writeup
  pageObjs <- parseObj pages


  --- Convert index.
  let
    cates =
      (\x -> mconcat
          [ M.singleton "cateName" (ObjLeaf x)
          , M.singleton "posts"
                (toNodeList (filter ((== Just x) . getCategory) postObjs))
          ]
        )
        <$> catMaybes (nub $ getCategory <$> postObjs)

    writeupcates =
      (\x -> mconcat
          [ M.singleton "cateName" (ObjLeaf x)
          , M.singleton "posts"
                (toNodeList (filter ((== Just x) . getCategory) writeupObjs))
          ]
        )
        <$> catMaybes (nub $ getCategory <$> writeupObjs)

  indexHtml <- do
    let indexObjTree = addGlb glbRes $ ObjNode $ mconcat
          [ M.singleton "posts" (toNodeList postObjs)
          , M.singleton "categories" (ObjNodeList cates)
          ]
    let indexTP = convertTP indexObjTree
          <$> T.readFile (themePath </> "layout/index.html")
    getFromTP "index" =<< indexTP

  diaryHtml <- do
    let diaryObjTree = addGlb glbRes $ ObjNode $ mconcat
          [ M.singleton "posts" (toNodeList diaryObjs)
          ]
    let diaryTP = convertTP diaryObjTree
          <$> T.readFile (themePath </> "layout/diary-index.html")
    getFromTP "diary-index" =<< diaryTP

  writeupHtml <- do
    let writeupObjTree = addGlb glbRes $ ObjNode $ mconcat
          [ M.singleton "posts" (toNodeList writeupObjs)
          , M.singleton "categories" (ObjNodeList writeupcates)
          ]
    let writeupTP = convertTP writeupObjTree
          <$> T.readFile (themePath </> "layout/writeup-index.html")
    getFromTP "writeup-index" =<< writeupTP

  -- Remove out-dated public dir.
  _ <- removeDirContent (outputDir cfg)

  -- Copy static files.
  let outputPath = outputDir cfg
  copyFiles outputPath (themePath </> "static/") statics
  copyFiles outputPath (atclPath </> "static/")  cStatics

  let articles = addGlb glbRes <$> runAtclModule (postObjs <> pageObjs <> diaryObjs <> writeupObjs)
  -- Generate htmls.
  
  gnrtSitemap outputPath (siteUrl cfg) $
      addGlb glbRes <$> runAtclModule (postObjs <> writeupObjs)
  
  gnrtHtmls outputPath themePath articles          -- Posts and pages
  T.writeFile (outputDir cfg </> "index.html") indexHtml  -- Index
  createDirectoryIfMissing True (outputDir cfg </> "diary/")
  T.writeFile (outputDir cfg </> "diary/index.html") diaryHtml  -- Index
  createDirectoryIfMissing True (outputDir cfg </> "writeup/")
  T.writeFile (outputDir cfg </> "writeup/index.html") writeupHtml  -- Index


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
