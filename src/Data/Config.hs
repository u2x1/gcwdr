{-# LANGUAGE OverloadedStrings #-}
module Data.Config where

import           Toml (TomlCodec, (.=))
import qualified Toml

import           Data.Config.Type

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.text "title"              .= siteTitle
  <*> Toml.text "site"               .= siteUrl
  <*> Toml.list menuC "menu"         .= siteMenus
  <*> Toml.string "outputDir"        .= outputDir
  <*> Toml.string "themeDir"         .= themeDir
  <*> Toml.string "articleDir"       .= articleDir
  <*> Toml.int "localServerPort"  .= localServerPort


menuC :: TomlCodec Menu
menuC = Menu
  <$> Toml.text "name" .= menuName
  <*> Toml.text "loc"  .= menuLoc
