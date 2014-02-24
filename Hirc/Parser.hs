{-# LANGUAGE OverloadedStrings #-}
module Hirc.Parser
   ( ircparser ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)

import Hirc.Types

ircparser :: Parser IRC
ircparser = A.choice [privmsg, ping, numeric, join]

-- Parse nick, user and host
-- :nick!user@host
user :: Parser (Text, Text, Text)
user = do
    A.char ':'
    n<- A.takeWhile (/= '!')
    A.char '!'
    u<- A.takeWhile (/= '@')
    A.char '@'
    h<- A.takeWhile (/= ' ')
    A.space
    return (n, u, h)

-- :nick!user@host PRIVMSG dest :text
privmsg :: Parser IRC
privmsg = do
    (ni, us, hs) <- user
    A.string "PRIVMSG"
    A.space
    d <- A.takeWhile1 (/= ' ')
    A.space
    A.char ':'
    t <- A.takeText
    return $ Privmsg ni us hs d t

-- PING :server.irc
ping :: Parser IRC
ping = do
    A.string "PING"
    A.string " :"
    s <- A.takeText
    return $ Ping s

-- :nick!user@host JOIN :#channel
join :: Parser IRC
join = do
    (ni, us, hs) <- user
    A.string "JOIN"
    A.space
    A.char ':'
    ch <- A.takeText
    return $ Join ni us hs ch

-- Parse numeric commands
numeric :: Parser IRC
numeric = do
    A.char ':'
    A.takeWhile (/= ' ')
    A.char ' '
    n <- A.takeWhile1 (`elem` ['0'..'9'])
    r <- A.takeText
    return $ Numeric n (Just r)
