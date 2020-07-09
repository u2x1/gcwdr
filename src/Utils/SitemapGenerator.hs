{-# LANGUAGE OverloadedStrings #-}
module Utils.SitemapGenerator where

import Data.ByteString
import Data.ByteString.UTF8 (fromString)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import Data.Maybe (catMaybes)

import Data.Template
import Data.Template.Type

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

getSitemap :: ByteString -> [ObjectTree] -> IO ByteString
getSitemap site objs = do
  let infos = catMaybes $ getUrlInfo <$> objs
  curTime <- (fromString . (formatTime defaultTimeLocale "%Y-%m-%d")) <$> getCurrentTime
  let siteUrl = URLInfo site curTime 8
  let sitemap = packSitemap site (siteUrl : infos)
  pure sitemap

getUrlInfo :: ObjectTree -> Maybe URLInfo
getUrlInfo obj = URLInfo <$> l <*> m <*> p
  where l = getNode "this" obj >>= getLeaf' "relLink"
        m = fromString <$> (fmap (formatTime defaultTimeLocale "%Y-%m-%d") (getDate =<< (getNode "this" obj)))
        p = Just 6
