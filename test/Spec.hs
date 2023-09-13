{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Data.Attoparsec.Text           ( many'
                                                , parseOnly
                                                )
import           Data.Markdown.Parser           ( mdElem )
import           Data.Markdown.Type
import           Data.Template.Parser           ( stmt )
import           Data.Template.Type             ( Stmt(..) )
import           Data.Text                      ( Text )
import           Prelude                 hiding ( unlines )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Markdown parsing stuff" $ do
    it "parses bold" $ do
      parseMD "**test**" `shouldBe` rtPara [Bold "test"]
    it "parses italic" $ do
      parseMD "*test*" `shouldBe` rtPara [Italic "test"]
    it "parses boldAndItalic" $ do
      parseMD "***test***" `shouldBe` rtPara [BoldAndItalic "test"]
    it "parses raw html tag" $ do
      parseMD "<style>ababa</style>" `shouldBe` rt [RawHtmlTag "style" "" "ababa"]
    it "parses raw html tag with props" $ do
      parseMD "<style type=\"text/css\" scoped>ababa</style>"
        `shouldBe` rt [RawHtmlTag "style" "type=\"text/css\" scoped" "ababa"]
    it "parses unordered list" $ do
      parseMD "- xyz\n- zyx"
        `shouldBe` rt
                     [ UnorderedList
                         [ ListElem [PlainText "xyz"]
                         , ListElem [PlainText "zyx"]
                         ]
                     ]
    it "parses expandBlock" $ do
      parseMD "$$$detail\nhahaha\n$$$"
        `shouldBe` rt [ExpandBlock "detail" [Paragrah [PlainText "hahaha"]]]
    it "parses latex inline" $ do
      parseMD "$formula$"
        `shouldBe` rt [Paragrah [LatexInline "formula"]]
      parseMD "$$"
        `shouldBe` rt [Paragrah [PlainText "$$"]]
    it "parses latex block" $ do
      parseMD "$$formula$$"
        `shouldBe` rt [Paragrah [LatexBlock "formula"]]
      parseMD "$$$$"
        `shouldBe` rt [Paragrah [PlainText "$$$$"]]

  describe "Template parsing stuff" $ do
    it "parses dot" $ do
      parseTP "[- this.posts -]" `shouldBe` rt [DotStmt ["this", "posts"]]
      parseTP "[- this. -]" `shouldBe` rt [Raw "[- this. -]"]
    it "parses partials" $ do
      parseTP "[- partial index.html -]"
        `shouldBe` rt [PartialStmt "index.html"]
    it "parses foreach" $ do
      parseTP "[- foreach x in this.posts -][- end -]"
        `shouldBe` rt [ForeachStmt "x" (DotStmt ["this", "posts"]) []]
      parseTP "[- foreach x in this.posts -][- x.title -][- end -]"
        `shouldBe` rt
                     [ ForeachStmt "x"
                                   (DotStmt ["this", "posts"])
                                   [DotStmt ["x", "title"]]
                     ]

    it "parses ifdef" $ do
      parseTP "[- ifdef this.posts -][- x.title -][- end -]"
        `shouldBe` rt [IfdefStmt (DotStmt ["this", "posts"]) [DotStmt ["x", "title"]] []]
      parseTP "[- ifdef this.posts -][- else -][- end -]"
        `shouldBe` rt [IfdefStmt (DotStmt ["this", "posts"]) [] []]
      parseTP
          "[- ifdef this.posts -][- x.title -][- else -][- y.title -][- end -]"
        `shouldBe` rt
                     [ IfdefStmt (DotStmt ["this", "posts"])
                                 [DotStmt ["x", "title"]]
                                 [DotStmt ["y", "title"]]
                     ]


rtPara :: [MDElem] -> Either a [MDElem]
rtPara x = Right [Paragrah x]

rt :: b -> Either a b
rt = Right

parseTP :: Text -> Either String [Stmt]
parseTP = parseOnly (many' stmt)

parseMD :: Text -> Either String [MDElem]
parseMD = parseOnly (many' mdElem) . (<> "\n")
