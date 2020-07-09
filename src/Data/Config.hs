{-# LANGUAGE OverloadedStrings #-}
module Data.Config where

import           Toml (TomlCodec, (.=))
import qualified Toml

import           Data.Config.Type

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.byteString "title" .= siteTitle
  <*> Toml.byteString "site"  .= siteUrl
  <*> Toml.list menuC "menu"  .= siteMenus


menuC :: TomlCodec Menu
menuC = Menu
  <$> Toml.byteString "name" .= menuName
  <*> Toml.byteString "loc"  .= menuLoc
