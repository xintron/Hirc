{-# LANGUAGE OverloadedStrings #-}
module Hirc.Hooks where

import Control.Monad.State
import Data.Monoid ((<>))
import Database.SQLite.Simple

import Hirc.Bot
import Hirc.Types

hooks :: [Hook]
hooks = [seen]

seen :: Hook
seen (Privmsg n _ _ d t)= do
    conn <- get
    let db = connDb conn
        name = servName $ connServer conn
    io $ execute db "INSERT OR REPLACE INTO seen \
        \(channel, nick, message, timestamp) \
        \VALUES (?, ?, ?, datetime('now'))" (name <> d, n, t)
