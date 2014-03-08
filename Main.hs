{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple
import System.IO

import Config
import Hirc.Bot
import Hirc.Types

main :: IO ()
main = do
    conns <- newMVar $ Connections []
    db <- open $ confDatabase config
    forM_ servers $ \s -> do
        chan <- newChan
        forkIO $ withFile (servLogFile s) AppendMode $ printer chan
        forkIO $ initialize conns chan db s
    userInput conns
    return ()

printer :: Chan T.Text -> Handle -> IO ()
printer chan handle = do
    hSetBuffering handle LineBuffering
    forever $ readChan chan >>= T.hPutStrLn handle


userInput :: MVar Connections -> IO ()
userInput connections = forever $ do
    input <- T.getLine
    -- Take the list of connections (block threads doing updates to this list)
    conns <- takeMVar connections
    putMVar connections conns
    let (chan, text) = T.break (== ' ') input
        conn = head $ connsServers conns
        h = connHandle conn
        t = "PRIVMSG " <> chan <> " :" <> tail' text
    writeChan (connIOChannel conn) t
    T.hPutStrLn h t
  where
    tail' "" = ""
    tail' t = T.tail t
