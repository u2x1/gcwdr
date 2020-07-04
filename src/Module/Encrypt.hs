{-# LANGUAGE OverloadedStrings #-}
module Module.Encrypt where

import Data.ByteString

advance :: ByteString -> ByteString -> ByteString
advance passwd origin = pack $ fmap (\(a, b) -> a + b) s
  where s = Prelude.zip (mconcat.repeat $ unpack passwd) (unpack origin)

-- vaildChars :: [Word8]
vaildChars = [48..127]
