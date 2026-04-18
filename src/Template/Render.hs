module Template.Render where

import           Data.Attoparsec.Text           ( IResult(..)
                                                , many'
                                                , parse
                                                )
import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Data.List                      ( intercalate )
import           Data.Map.Lazy                 as M
                                                ( (!?)
                                                , Map
                                                , insert
                                                , keys
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Template.Parser                ( stmt )
import           Template.Type                  ( ObjectTree(..)
                                                , Stmt(..)
                                                , getType
                                                , showObjTree
                                                )
import           Article.Query                  ( getNode
                                                , getLeaf'
                                                )

convertTP :: ObjectTree -> Text -> Either String Text
convertTP objTree s =
  case parseStmts (s <> "\n") of
    Right xs -> concatAndInit $ convertTP' objTree <$> xs
    Left  e  -> Left e

parseStmts :: Text -> Either String [Stmt]
parseStmts input =
  case parse (many' stmt) input of
    Done _ xs        -> Right xs
    Partial k        -> case k "" of
      Done _ xs      -> Right xs
      Fail rest ctx msg -> Left $ formatParseFail input rest ctx msg
      Partial _      -> Left "template parse error: unexpected partial input"
    Fail rest ctx msg -> Left $ formatParseFail input rest ctx msg

formatParseFail :: Text -> Text -> [String] -> String -> String
formatParseFail input rest ctx msg =
  let offset      = T.length input - T.length rest
      prefix      = T.take offset input
      lineNum     = T.count "\n" prefix + 1
      afterLastNl = snd (T.breakOnEnd "\n" prefix)
      colNum      = T.length afterLastNl + 1
      lineStart   = offset - T.length afterLastNl
      lineText    = T.takeWhile (/= '\n') (T.drop lineStart input)
      ctxText     = if null ctx then "" else "  context: " <> intercalate " > " ctx
      caretLine   = "  " <> replicate (max 0 (colNum - 1)) ' ' <> "^"
      hint        =
        "Hint: check control blocks are well-formed and closed with [- end -]; check [- else -]/[- end -] pairing; check if conditions use quoted strings."
  in  unlines
        [ "template parse error at line " <> show lineNum <> ", col " <> show colNum
        , ""
        , "  " <> T.unpack lineText
        , caretLine
        , ""
        , "attoparsec: " <> msg <> ctxText
        , hint
        ]

concatAndInit :: [Either String Text] -> Either String Text
concatAndInit x = if Prelude.null $ lefts x
  then Right $ concatAndInit' (rights x)
  else Left $ unlines $ lefts x

concatAndInit' :: [Text] -> Text
concatAndInit' x | mconcat x == ""            = mconcat x
                 | T.last (mconcat x) == '\n' = T.init $ mconcat x
                 | otherwise                  = mconcat x

convertTP' :: ObjectTree -> Stmt -> Either String Text
convertTP' _    (Raw rawHtml)   = Right rawHtml
convertTP' objs s@IfdefStmt{}   = convertIfdef objs s
convertTP' objs s@IfEqStmt{}    = convertIfEq objs s
convertTP' objs s@ForeachStmt{} = convertForeach objs s
convertTP' objs s@DotStmt{}     = convertDot objs s
convertTP' objs s@PartialStmt{} = convertPartial objs s

-- Converting template to the actual html.

convertPartial :: ObjectTree -> Stmt -> Either String Text
convertPartial objs (PartialStmt partPath) =
  case getNode "global" objs >>= getNode "partials" >>= getLeaf' partPath of
    Just partFile -> case convertTP objs partFile of
      Right x   -> Right x
      Left  err -> Left $ "converting partial " <> show partPath <> ": " <> err
    _ ->
      Left $ "partial file " <> show partPath <> " not found in global resource"
convertPartial _ s = Left ("unexpected statement: " <> (show s))


convertIfdef :: ObjectTree -> Stmt -> Either String Text
convertIfdef objs@(ObjNode _) (IfdefStmt (DotStmt dotObj) trueStmts falseStmts)
  = if checkIfExist objs dotObj
    then go $ fmap (convertTP' objs) trueStmts
    else go $ fmap (convertTP' objs) falseStmts
 where
  go x = if Prelude.null $ lefts x
    then Right $ mconcat . rights $ x
    else Left $ unlines $ lefts x
  checkIfExist _           []       = True
  checkIfExist (ObjNode o) (d : ds) = case o !? d of
    Just x -> checkIfExist x ds
    _      -> False
  checkIfExist _ _ = False
convertIfdef _ s = Left ("unexpected statement: " <> (show s))

convertIfEq :: ObjectTree -> Stmt -> Either String Text
convertIfEq objs (IfEqStmt val (DotStmt dotObj) trueStmts falseStmts negated) =
  case resolveLeaf objs dotObj of
    Just v  -> if (if negated then v /= val else v == val) then go trueStmts else go falseStmts
    Nothing -> if negated then go trueStmts else go falseStmts
 where
  go stmts = if Prelude.null $ lefts results
    then Right $ mconcat $ rights results
    else Left $ unlines $ lefts results
   where results = fmap (convertTP' objs) stmts
  resolveLeaf :: ObjectTree -> [Text] -> Maybe Text
  resolveLeaf (ObjNode o) (d : ds) = case o !? d of
    Just (ObjLeaf v)     -> if null ds then Just v else Nothing
    Just (ObjNode inner) -> case ds of
      []     -> Nothing
      (_:_)  -> resolveLeaf (ObjNode inner) ds
    _ -> Nothing
  resolveLeaf _ _ = Nothing
convertIfEq _ s = Left ("unexpected statement: " <> (show s))

convertForeach :: ObjectTree -> Stmt -> Either String Text
convertForeach objs@(ObjNode objs') (ForeachStmt holder dotObj stmts) =
  case convertDot2NodeList objs dotObj of
    Right nodeList -> go . mconcat $ fmap
      (\node -> fmap (convertTP' (addRes node)) stmts)
      nodeList
    Left err -> Left $ "converting nodelist: " <> err
 where
  addRes node' = ObjNode (M.insert holder (ObjNode node') objs')
  go x = if Prelude.null $ lefts x
    then Right $ mconcat . rights $ x
    else Left $ unlines $ lefts x
convertForeach _ s = Left ("unexpected statement: " <> (show s))

objKeys :: ObjectTree -> [Text]
objKeys (ObjNode m) = M.keys m
objKeys _           = []

missingScopeHint :: [Text] -> Text -> [Text] -> String
missingScopeHint mems missing availableKeys = case mems of
  (h : _)
    | h == missing
    , "this" `elem` availableKeys
    , "global" `elem` availableKeys
    -> "\nHint: \"" <> T.unpack missing <> "\" is not in scope. If this is a foreach placeholder, a surrounding foreach/if/ifdef may have ended early due to a malformed tag or mismatched [- end -]/[- else -]."
  _ -> ""

convertDot2NodeList
  :: ObjectTree -> Stmt -> Either String [Map Text ObjectTree]
convertDot2NodeList objs (DotStmt mems) =
  let run (ObjNodeList s  ) []       = Right s
      run (ObjNode     obj) (x : xs) = case obj !? x of
        Just a -> run a xs
        _      ->
          Left
            (  show x
            <> " on "
            <> show mems
            <> " not found in object tree:\n"
            <> showObjTree objs
            <> missingScopeHint mems x (M.keys obj)
            )
      run obj x =
        (Left
          (  "cannot match the expected type ObjNode with actual type "
          <> getType obj
          <> " in "
          <> show x
          )
        )
  in  run objs mems
convertDot2NodeList _ s = Left ("unexpected statement: " <> (show s))

convertDot :: ObjectTree -> Stmt -> Either String Text
convertDot objs (DotStmt mems) =
  let run (ObjLeaf s  ) []       = Right s
      run (ObjNode obj) (x : xs) = case obj !? x of
        Just a -> run a xs
        _ ->
          Left
            (  show x
            <> " on "
            <> show mems
            <> " not found in "
            <> showObjTree objs
            <> missingScopeHint mems x (M.keys obj)
            )
      run obj x =
        (Left
          (  "cannot match the expected type ObjLeaf with actual type "
          <> getType obj
          <> " in "
          <> show x
          )
        )
  in  run objs mems
convertDot _ s = Left ("unexpected statement: " <> (show s))
