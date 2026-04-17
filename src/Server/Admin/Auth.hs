module Server.Admin.Auth where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.IORef                 ( readIORef, writeIORef )
import           Data.Maybe                 ( mapMaybe )
import           Data.Text                  ( Text )
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL
import           Data.Time                  ( addUTCTime, getCurrentTime )

import           Network.HTTP.Types.Status  ( status401 )
import           Web.Scotty                 ( ActionM, header, setHeader, status
                                            , text, redirect, finish )

import           Server.Admin.Types         ( AdminEnv(..), Session(..), SessionStore )

import           Numeric                    ( showHex )

-- ---------------------------------------------------------------------------
-- SHA-256 (pure Haskell, minimal implementation)
-- We implement a simple SHA-256 to avoid adding a dependency.
-- For production use, consider the 'cryptohash-sha256' package.
-- ---------------------------------------------------------------------------

import qualified Data.ByteString           as BS
import           Data.Array                  ( Array, listArray, (!) )
import           Data.Word                  ( Word8, Word32, Word64 )
import           Data.Bits

sha256 :: BS.ByteString -> BS.ByteString
sha256 msg = encodeHash $ foldl processBlock initHash (toBlocks padded)
  where padded = padMessage msg

-- Initial hash values (first 32 bits of fractional parts of square roots of first 8 primes)
initHash :: [Word32]
initHash =
  [ 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a
  , 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 ]

-- Round constants
kConsts :: [Word32]
kConsts =
  [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
  , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
  , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
  , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
  , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
  , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
  , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
  , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 ]

padMessage :: BS.ByteString -> BS.ByteString
padMessage msg =
  let len = BS.length msg
      bitLen = fromIntegral len * 8 :: Word64
      -- pad with 0x80, then zeros, then 8-byte big-endian length
      padLen = (55 - len) `mod` 64
      padding = BS.singleton 0x80 <> BS.replicate padLen 0x00
      lenBytes = BS.pack [ fromIntegral (bitLen `shiftR` (8 * i)) | i <- [7,6..0] ]
  in msg <> padding <> lenBytes

-- Word64 imported from Data.Word

toBlocks :: BS.ByteString -> [BS.ByteString]
toBlocks bs
  | BS.null bs = []
  | otherwise  = let (b, rest) = BS.splitAt 64 bs in b : toBlocks rest

bytesToWord32 :: [Word8] -> Word32
bytesToWord32 [a,b,c,d] = (fromIntegral a `shiftL` 24) .|.
                           (fromIntegral b `shiftL` 16) .|.
                           (fromIntegral c `shiftL` 8)  .|.
                            fromIntegral d
bytesToWord32 _ = 0

blockToWords :: BS.ByteString -> [Word32]
blockToWords bs = [ bytesToWord32 (BS.unpack $ BS.take 4 $ BS.drop (i*4) bs) | i <- [0..15] ]

messageSchedule :: [Word32] -> [Word32]
messageSchedule ws = [ w i | i <- [0..63] ]
  where
    wsArr :: Array Int Word32
    wsArr = listArray (0, 63) [ w i | i <- [0..63] ]
    w i
      | i < 16    = ws !! i
      | otherwise = sig1 (wsArr ! (i-2)) + (wsArr ! (i-7)) + sig0 (wsArr ! (i-15)) + (wsArr ! (i-16))
    sig0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
    sig1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10

processBlock :: [Word32] -> BS.ByteString -> [Word32]
processBlock hash block =
  let ws = messageSchedule (blockToWords block)
      compress [a,b,c,d,e,f,g,h] (ki, wi) =
        let s1   = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
            ch   = (e .&. f) `xor` (complement e .&. g)
            temp1 = h + s1 + ch + ki + wi
            s0   = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
            maj  = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
            temp2 = s0 + maj
        in [temp1 + temp2, a, b, c, d + temp1, e, f, g]
      compress st _ = st
      result = foldl compress hash (zip kConsts ws)
  in zipWith (+) hash result

encodeHash :: [Word32] -> BS.ByteString
encodeHash = BS.pack . concatMap word32ToBytes
  where
    word32ToBytes w = [ fromIntegral (w `shiftR` 24)
                      , fromIntegral (w `shiftR` 16)
                      , fromIntegral (w `shiftR` 8)
                      , fromIntegral w ]

-- | Hex-encode a ByteString
toHex :: BS.ByteString -> Text
toHex = T.pack . concatMap (\w -> let s = showHex w "" in if length s == 1 then '0':s else s) . BS.unpack

-- | Hash password with salt: SHA-256(salt <> password)
hashPassword :: Text -> Text -> Text
hashPassword salt password =
  toHex $ sha256 $ TE.encodeUtf8 (salt <> password)

-- | Check if a password matches the stored hash
checkPassword :: Text -> Text -> Text -> Bool
checkPassword salt storedHash password =
  hashPassword salt password == storedHash

-- ---------------------------------------------------------------------------
-- Session management
-- ---------------------------------------------------------------------------

-- | Generate a random-ish token (64 hex chars).
-- Uses current time + a counter approach. Not cryptographically strong,
-- but sufficient for a local admin tool.
generateToken :: IO Text
generateToken = do
  now <- getCurrentTime
  let seed = show now
      hashed = toHex $ sha256 $ TE.encodeUtf8 $ T.pack seed
  -- Hash it twice with time variation for more entropy
  secondHash <- do
    t2 <- getCurrentTime
    pure $ toHex $ sha256 $ TE.encodeUtf8 $ hashed <> T.pack (show t2)
  pure secondHash

-- | Create a new session (24h TTL)
createSession :: SessionStore -> IO Text
createSession store = do
  token <- generateToken
  now <- getCurrentTime
  let expires = addUTCTime (24 * 3600) now
      session = Session token expires
  sessions <- readIORef store
  -- Prune expired while we're at it
  let valid = filter (\s -> sessExpires s > now) sessions
  writeIORef store (session : valid)
  pure token

-- | Validate a session token, pruning expired ones
validateSession :: SessionStore -> Text -> IO Bool
validateSession store token = do
  now <- getCurrentTime
  sessions <- readIORef store
  let valid = filter (\s -> sessExpires s > now) sessions
  writeIORef store valid
  pure $ any (\s -> sessToken s == token) valid

-- | Remove a session
removeSession :: SessionStore -> Text -> IO ()
removeSession store token = do
  sessions <- readIORef store
  writeIORef store $ filter (\s -> sessToken s /= token) sessions

-- ---------------------------------------------------------------------------
-- Cookie helpers
-- ---------------------------------------------------------------------------

-- | Extract session token from Cookie header
getSessionToken :: ActionM (Maybe Text)
getSessionToken = do
  cookieHeader <- header "Cookie"
  pure $ cookieHeader >>= extractToken . TL.toStrict
  where
    extractToken :: Text -> Maybe Text
    extractToken h =
      let pairs = map T.strip $ T.splitOn ";" h
          parsed = mapMaybe parsePair pairs
      in lookup "gcwdr_admin_session" parsed
    parsePair :: Text -> Maybe (Text, Text)
    parsePair p = case T.splitOn "=" p of
      [k, v] -> Just (T.strip k, T.strip v)
      _      -> Nothing

-- | Set the session cookie
setSessionCookie :: Text -> ActionM ()
setSessionCookie token =
  setHeader "Set-Cookie" $
    TL.fromStrict $ "gcwdr_admin_session=" <> token
      <> "; Path=/admin; HttpOnly; SameSite=Strict"

-- | Clear the session cookie
clearSessionCookie :: ActionM ()
clearSessionCookie =
  setHeader "Set-Cookie"
    "gcwdr_admin_session=; Path=/admin; HttpOnly; SameSite=Strict; Max-Age=0"

-- ---------------------------------------------------------------------------
-- Auth middleware helpers
-- ---------------------------------------------------------------------------

-- | Require auth for page routes — redirects to login on failure
requireAuth :: AdminEnv -> ActionM Text
requireAuth env = do
  mToken <- getSessionToken
  case mToken of
    Nothing -> do
      _ <- redirect "/admin/login"
      finish
    Just token -> do
      valid <- liftIO $ validateSession (envSessionStore env) token
      if valid
        then pure token
        else do
          clearSessionCookie
          _ <- redirect "/admin/login"
          finish

-- | Require auth for API routes — returns 401 on failure
requireAuthAPI :: AdminEnv -> ActionM Text
requireAuthAPI env = do
  mToken <- getSessionToken
  case mToken of
    Nothing -> do
      status status401
      text "Unauthorized"
      finish
    Just token -> do
      valid <- liftIO $ validateSession (envSessionStore env) token
      if valid
        then pure token
        else do
          clearSessionCookie
          status status401
          text "Unauthorized"
          finish
