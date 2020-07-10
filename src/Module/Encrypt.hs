{-# LANGUAGE OverloadedStrings #-}
module Module.Encrypt where

import Data.Text.Lazy as TL (pack, zip, Text, pack)
import Data.Char            (ord, chr)

advance :: Text -> Text -> Text
advance passwd origin = pack $ fmap (\(a, b) -> charPlus a b) s
  where s = TL.zip (mconcat.repeat $ passwd) origin

charPlus :: Char -> Char -> Char
charPlus a b = chr $ (ord a) + (ord b)
