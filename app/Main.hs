{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent
import Entry.Read
import System.IO
import Network.Socket
import qualified Control.Exception as E
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.UTF8 (toString)

main :: IO ()
main = do
  trans "./test-data"
  runTCPServer Nothing "4000" talk
  where
    talk s = do
        msg <- recv s 1024
        unless (S.null msg) $
          if S.take 3 msg == "GET"
             then do
               resp <- getFile "./test-data/public" (toString $ S.takeWhile (/= 32) (S.drop 4 msg))
               sendAll s ("HTTP/1.1 200 OK\n\n" <> resp)
             else sendAll s "HTTP/1.1 200 OK\n\n"

getFile :: String -> String -> IO S.ByteString
getFile root path
  | last path == '/' = S.readFile (root <> path <> "index.html")
  | otherwise = S.readFile (root <> path)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer host port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = head <$> getAddrInfo (Just defaultHints) host (Just port)
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
