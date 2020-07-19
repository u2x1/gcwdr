{-# LANGUAGE OverloadedStrings #-}
module Utils.Server where

import           Control.Monad
import           Control.Concurrent
import qualified Control.Exception         as E
import           Network.Socket
import           Network.Socket.ByteString        (recv, sendAll)
import qualified Data.ByteString           as S
import           Data.ByteString.UTF8             (toString)
import           Utils.Logging

runBlogPreview :: FilePath -> Int -> IO ()
runBlogPreview root port = do
  logWT Info $ "running web server at http://localhost:" <> show port
  runTCPServer (show port) (showHtml root)
  where
    showHtml path s = do
        msg <- recv s 1024
        unless (S.null msg) $
          if S.take 3 msg == "GET"
             then do
               let fileName = let name = (toString $ S.takeWhile (/= 32) (S.drop 4 msg)) in
                                if last name /= '/'
                                   then path <> name
                                   else path <> name <> "index.html"
               fCntnt <- S.readFile fileName
               let status = "HTTP/1.1 200 OK\n"
               let resp = status <> "\n" <> fCntnt
               sendAll s resp
             else sendAll s "HTTP/1.1 500"

runTCPServer :: ServiceName -> (Socket -> IO a) -> IO a
runTCPServer port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = head <$> getAddrInfo (Just defaultHints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

-- getFileType :: String -> S.ByteString
-- getFileType path = case takeWhileEnd (/= '.') path of
--                      "html" -> "text/html"
--                      "css"  -> "text/css"
--                      "jpg"  -> "image/jpeg"
--                      "png"  -> "image/png"
--                      "gif"  -> "image/gif"
--                      "tiff" -> "image/tiff"
--                      _      -> "text/plain"
--   where takeWhileEnd f xs = reverse $ go f (reverse xs)
--         go g (y:ys) = if g y then y : (go g ys) else []
--         go _ [] = []
