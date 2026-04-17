module Server.Admin.Templates where

import qualified Data.Map.Lazy             as M
import           Data.Text                  ( Text )
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.FilePath            ( (</>) )

import           Config.Type                ( Config(..) )
import           Template.Render            ( convertTP )
import           Template.Type              ( ObjectTree(..) )

-- | Render an admin template with two-pass rendering:
-- 1. Render the page template with the given variables → body_content
-- 2. Inject body_content into base.html
renderAdminTemplate
  :: Config
  -> Text              -- ^ Template name (e.g., "dashboard")
  -> M.Map Text ObjectTree  -- ^ Template variables
  -> IO (Either String Text)
renderAdminTemplate cfg templateName vars = do
  let adminDir = themeDir cfg </> "layout/admin"
  -- Read page template
  pageTPL <- TIO.readFile (adminDir </> T.unpack templateName <> ".html")
  -- Read base template
  baseTPL <- TIO.readFile (adminDir </> "base.html")

  -- Read partials (nav)
  navContent <- TIO.readFile (adminDir </> "partials/nav.html")

  -- First pass: render page template with variables
  let pageTree = ObjNode $ M.insert "partials"
                   (ObjNode (M.singleton "nav.html" (ObjLeaf navContent)))
                   (M.singleton "this" (ObjNode vars))
  case convertTP pageTree pageTPL of
    Left errs -> pure $ Left errs
    Right bodyContent -> do
      -- Second pass: inject body_content into base.html
      let baseVars = M.insert "body_content" (ObjLeaf bodyContent) vars
          baseTree = ObjNode $ M.insert "partials"
                      (ObjNode (M.singleton "nav.html" (ObjLeaf navContent)))
                      (M.singleton "this" (ObjNode baseVars))
      pure $ convertTP baseTree baseTPL
