{-# LANGUAGE OverloadedStrings #-}
module SimpleSpec where

import Data.ByteString
import Data.Attoparsec.ByteString
import Test.Hspec
import Data.Markdown.Type
import Data.Markdown.Parser
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

rtPara :: [MDElem] -> Either a [MDElem]
rtPara x = Right [Paragrah x]

rt :: b -> Either a b
rt x = Right x

parseMD :: ByteString -> Either String [MDElem]
parseMD = parseOnly (many' mdElem) . (<>"\n")
