{-# LANGUAGE OverloadedStrings #-}
module SimpleSpec where

import Data.Attoparsec.ByteString
import Data.ByteString.Search as BS
import Data.ByteString        as BS (ByteString)
import Data.ByteString.Char8        (unlines)
import Test.Hspec
import Test.QuickCheck
import Markdown.Parser
import Markdown.Type
import Markdown.Convert
import Prelude hiding (unlines)

spec :: Spec
spec = do
  describe "Parser.convertMD" $ do
    it "parses bold" $ do
      parseMD "**test**" `shouldBe` (rtPara [Bold "test"])
    it "parses italic" $ do
      parseMD "*test*" `shouldBe` (rtPara [Italic "test"])
    it "parses boldAndItalic" $ do
      parseMD "***test***" `shouldBe` (rtPara [BoldAndItalic "test"])

  describe "Mixed Element" $ do
    it "parses unordered list" $ do
      parseMD "- xyz\n- zyx" `shouldBe` (rt $
        [ UnorderedList [ ListElem [PlainText "xyz"]
                        , ListElem [PlainText "zyx"]]])

rtPara x = Right [Paragrah x]
rt x = Right x
parseMD = parseOnly (many' mdElem) . (<>"\n")
