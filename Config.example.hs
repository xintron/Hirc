{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text as T

import Hirc.Types
import Hirc.Plugins

server :: Server
server = Server
    { servHost = "localhost"
    , servPort = 6666
    , servChans = [test]
    , servLogFile = "localhost.log"
    , servBotNick = "`foo"
    , servBotName = "Foo Bar" }

test :: Channel
test = Channel
    { chanName = "#test"
    , chanPrefix = "!"
    , chanPassword = T.empty
    , chanPlugins = plugins
    , chanHooks = [] }
