{-# LANGUAGE OverloadedStrings #-}
module Hirc.Bot where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Attoparsec.Text (parseOnly)
import Data.List as L
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.SQLite.Simple as SQ
import Network
import System.IO

import Hirc.Parser
import Hirc.Types

initialize :: MVar Connections -> Chan Text -> SQ.Connection -> Server -> IO ()
initialize connections ioChan db server = do
    handle <- connectTo
        (T.unpack $ servHost server)
        (PortNumber $ fromIntegral $ servPort server)
    -- Buffer by line
    hSetBuffering handle LineBuffering
    -- Set line mode to \r\n (only for network handle)
    hSetNewlineMode handle (NewlineMode CRLF CRLF)
    -- Use utf8 as encoding for the connection
    mkTextEncoding "UTF-8//TRANSLIT" >>= hSetEncoding handle
    -- Run the bot
    -- Add running server to Connections
    replyChan <- newChan
    c <- takeMVar connections
    let conn = Connection server handle db replyChan ioChan
    putMVar connections $ Connections (conn : connsServers c)
    forkIO $ void $ runStateT reply conn

    -- Send Nick and User to reply channel
    writeChan replyChan $ RawCommand nick
    writeChan replyChan $ RawCommand user

    -- Start reading
    void $ runStateT listen conn
  where
    nick = "NICK " <> servBotNick server
    user = let user' = servBotName server
        in "USER " <> user' <> " 12 * :" <> user' <> " - assistant"

-- Log data to stdout
lio :: Text -> StConn ()
lio c = get >>= io . flip writeChan c . connIOChannel

-- Write data over the connection (and log)
write :: Text -> StConn ()
write c = do
    lio $ "> " <> c
    liftM connHandle get >>= io . flip T.hPutStrLn c

reply :: StConn ()
reply = forever $ do
    ircrpl <- get >>= io . readChan . connChannel
    action ircrpl
  where
    action :: IRCRpl -> StConn ()
    action (RawCommand text) = write text
    action (RplPrivmsg dest text) = write $ "PRIVMSG " <> dest <> " :" <> text
    action _ = return ()

-- Loop for reading data over the connection and triggering 
listen :: StConn ()
listen = forever $ do
    cnn <- get
    c <- io $ T.hGetLine $ connHandle cnn
    lio $ "< " <> c
    -- Parse the incoming data
    case parseOnly ircparser c of
        Right irc -> runHandler irc
        Left _ -> return ()

runHandler :: IRC -> StConn ()
-- Got welcome message, join channels
runHandler (Numeric "001" _) = do
    c <- liftM (servChans . connServer) get
    chan <- liftM connChannel get
    io $ writeChan chan $
        RawCommand $ "JOIN " <>
            T.concat (L.intersperse "," $ chans c) <> " " <>
                T.concat (L.intersperse "," $ passwords c)
  where
    chans = L.map chanName
    passwords = filter (not . T.null) . L.map chanPassword
-- Do the Pong to da Ping
runHandler (Ping s) = write $ "PONG :" <> s
-- Run all plugins
runHandler cmd@(Privmsg _ _ _ dest text) = do
    conn <- get
    case find (\x -> T.toLower dest == T.toLower (chanName x)) $ servChans $ connServer conn of
        -- We got a channel match, run plugins
        Just chan -> do
            -- Fork and run all hooks
            -- TODO: Use worker thead and send in the hooks over a channel for
            -- the worker to handle.
            unless (null $ chanHooks chan)
                (io $ void $
                    forkIO $ evalStateT (runHooks (chanHooks chan) cmd) conn)
            -- Match keyword with text and run plugin if matching
            forM_ (chanPlugins chan) $ \(x, f) ->
                -- Match case insensitive. This could probably be handled
                -- better by only converting the first part of the words to
                -- lower and match against it but this works for now.
                when
                    (T.toLower (chanPrefix chan <> x)
                        `T.isPrefixOf` T.toLower text) $ f cmd
        Nothing -> return ()
runHandler _ = return ()

runHooks :: [Hook] -> IRC -> StConn ()
runHooks hooks irc = forM_ hooks ($ irc)

io :: IO a -> StConn a
io = liftIO
