{-# LANGUAGE OverloadedStrings #-}
module Utils.SitemapGenerator where

import Data.ByteString
import Data.ByteString.UTF8 (fromString)

packSitemap :: ByteString -> [URLInfo] -> ByteString
packSitemap site infos = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
  \<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
  <> mconcat ((info2xml site) <$> infos) <>
  "</urlset>"

info2xml :: ByteString -> URLInfo -> ByteString
info2xml site URLInfo{ loc = l, lastmod = m, priority = p} =
  "   <url>\n\
  \      <loc>" <> site <> l <> "</loc>\n\
  \      <lastmod>" <> m <> "</lastmod>\n\
  \      <priority>" <> if p > 9 then "1" else ("0." <> fromString (show p)) <> "</priority>\n\
  \   </url>\n"

data URLInfo = URLInfo {
    loc      :: ByteString
  , lastmod  :: ByteString
  , priority :: Int
} deriving (Show)
