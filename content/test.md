---
title: "Aeson Parsing Tips"
date: 2020-04-04T21:19:09+08:00
categories: "Tech"
---
##### Array with Objects

Suppose we have the JSON data:

```
{
  "media_id": "981538",
  "tags": [ {
      "id": 12227,
      "name": "english"
    }, {
      "id": 58971,
      "name": "japanese"
    } ]
}
```

We want it to be parsed into the following structure, with the `r_tags` taking only the name field of tags.
```
data Result = Result {
    r_id :: Text
  , r_tags :: [Text]
}
```

The easiest way is to parse via `newtype` wrapper:

```
data Result = Result {
    r_media_id :: Text
  , r_tags :: [RTag]
}
instance FromJSON Result where
  parseJSON = withObject "Result" $ \v -> Result
    <$> (v .: "media_id")
    <*> (v .: "tags")

newtype RTag = RTag { tag :: Text }
instance FromJSON RTag where
  parseJSON = withObject "RTag" $ \v ->
    RTag <$> ((v .: "name"))
```

But we can simplify this by removing the `RTag` wrapper. But first let’s take a look at some _Aeson_ types:

```
type Array = Vector Value
type Object = HashMap Text Value
parseJSON :: Value -> Parser a
(.:) :: FromJSON a => Object -> Text -> Parser a
```

And we can infer that `(v .: "tags")` parse tags into an Array(so has type Parser Array), inside which elements are parsed by the instance of `RTag`. Obviously, if we want to parse the tags directly into _[Text]_, we need to turn the type of `(v .: "tags")`, _Parser Array_, into _Parser [Text]_.

It can be done with a _do_ block, and a little help from `toList`:

```
import Data.Foldable (toList)
import Data.Aeson.Types (Parser)

parseJSON = withObject "Result" $ \v -> Result
  <$> (v .: "media_id")
  <*> do
    tagArray <- (v .: "tags") :: Parser Array
    tagObjs <- traverse (parseJSON :: Value -> Parser Object) $ toList tagArray :: Parser [Object]
    traverse (.: "name") tagObjs
```

Then remove _do_ block:

```
(v .: "tags") :: Parser Array >>= traverse parseJSON . toList >>= traverse (.: "name")
```

##### Field with different types

```
"result": [ {
    "id": 1531017
    "media_id": "148011"
  }, {
    "id": "5137523"
    "media_id": "135611"
  } ]
```

Look at the id field above, did you find that it can be either a _String_ or _Int_? It can be parsed using a `parseId` function.

```
parseId :: Value -> Parser Text
parseId (Number o) = pure $ (fst . Text.breakOn ".") $ Text.pack $ show o
parseId (String o) = pure o
parseId _ = mzero
```

With structure:

```
data Result = Result {
    r_wtf_id :: Text.Text
  , r_media_id :: Text.Text
}
instance FromJSON Result where
  parseJSON = withObject "Result" $ \v -> Result
    <$> ((v .: "id") >>= parseId)
    <*> (v .: "media_id")
```
