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
import           System.Directory

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
                                 path <> name <> if last name == '/' then "index.html" else ""
                exist <- doesFileExist fileName
                resp <- if exist
                          then do
                            fCntnt <- S.readFile fileName
                            return $ "HTTP/1.1 200 OK\r\n"
                                  <> "Content-Type: " <> getFileType fileName <> "; charset=utf-8 \r\n"
                                  <> "\r\n"
                                  <> fCntnt
                          else return "HTTP/1.1 400 Not Found"
                sendAll s resp
             else sendAll s "HTTP/1.1 501 Not Implemented"

runTCPServer :: ServiceName -> (Socket -> IO a) -> IO a
runTCPServer port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream } ) Nothing (Just port)
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

getFileType :: String -> S.ByteString
getFileType path = case takeWhileEnd (/= '.') path of
                     "html" -> "text/html"
                     "css"  -> "text/css"
                     "jpg"  -> "image/jpeg"
                     "png"  -> "image/png"
                     "gif"  -> "image/gif"
                     "tiff" -> "image/tiff"
                     "js"   -> "application/javascript"
                     _      -> "text/plain"
  where takeWhileEnd f xs = reverse $ go f (reverse xs)
        go g (y:ys) = if g y then y : go g ys else []
        go _ [] = []
