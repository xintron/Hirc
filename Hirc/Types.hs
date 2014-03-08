{-# LANGUAGE OverloadedStrings #-}
module Hirc.Types where

import Control.Concurrent.Chan
import Control.Monad.State
import Data.Text (Text)
import System.IO

import qualified Database.SQLite.Simple as SQ

data Connections = Connections { connsServers :: [Connection] }

data Connection = Connection { connServer :: Server
                             , connHandle :: Handle
                             , connDb :: SQ.Connection
                             , connChannel :: Chan IRCRpl
                             , connIOChannel :: Chan Text }

data Config = Config { confDatabase :: String }

data Server = Server
    { servName :: Text
    , servHost :: Text
    , servPort :: Int
    -- Channels with passwords needs to be put *before* channels without
    -- passwords
    , servChans :: [Channel]
    , servLogFile :: FilePath
    , servBotNick :: Text
    , servBotName :: Text }

data Channel = Channel
    { chanName :: Text
    , chanPrefix :: Text
    , chanPassword :: Text
    , chanPlugins :: [(Text, Plugin)]
    , chanHooks :: [Hook] }

data Nick = CI Text

data IRC = Ping { pingServer :: Text }
    | Privmsg { privNick :: Text
              , privName :: Text
              , privHost :: Text
              , privDest :: Text
              , privText :: Text }
    | Join { joinNick :: Text
           , joinName :: Text
           , joinHost :: Text
           , joinDest :: Text }
    | Numeric { numNumber :: Text
              , numText :: Maybe Text }

data IRCRpl = RplPing { rplPingServer :: Text }
    | RplPrivmsg { rplPrivDest :: Text
                 , rplPrivText :: Text }
    | RawCommand { rplRawCommand :: Text }

type StConn = StateT Connection IO
type Plugin = IRC -> StConn ()
type Hook = IRC -> StConn ()
