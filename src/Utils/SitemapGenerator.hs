{-# LANGUAGE OverloadedStrings #-}
module Utils.SitemapGenerator where

import Data.Text
import Data.Time           (getCurrentTime, defaultTimeLocale, formatTime)
import Data.Maybe          (catMaybes)

import Data.Template
import Data.Template.Type

packSitemap :: Text -> [URLInfo] -> Text
packSitemap site infos = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
  \<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
  <> mconcat ((info2xml site) <$> infos) <>
  "</urlset>"

info2xml :: Text -> URLInfo -> Text
info2xml site URLInfo{ loc = l, lastmod = m, priority = p} =
  "   <url>\n\
  \      <loc>" <> site <> l <> "</loc>\n\
  \      <lastmod>" <> m <> "</lastmod>\n\
  \      <priority>" <> if p > 9 then "1" else ("0." <> pack (show p)) <> "</priority>\n\
  \   </url>\n"

data URLInfo = URLInfo {
    loc      :: Text
  , lastmod  :: Text
  , priority :: Int
} deriving (Show)

getSitemap :: Text -> [ObjectTree] -> IO Text
getSitemap site objs = do
  let infos = catMaybes $ getUrlInfo <$> objs
  curTime <- (pack . (formatTime defaultTimeLocale "%Y-%m-%d")) <$> getCurrentTime
  let siteUrl = URLInfo "" curTime 8
  let sitemap = packSitemap site (siteUrl : infos)
  pure sitemap

getUrlInfo :: ObjectTree -> Maybe URLInfo
getUrlInfo obj = URLInfo <$> l <*> m <*> p
  where l = getNode "this" obj >>= getLeaf' "relLink"
        m = pack <$> (fmap (formatTime defaultTimeLocale "%Y-%m-%d") (getDate =<< (getNode "this" obj)))
        p = Just 6
