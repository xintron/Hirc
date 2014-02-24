{-# LANGUAGE OverloadedStrings #-}
module Hirc.Plugins where

import Control.Concurrent.Chan
import Control.Monad.State
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Hirc.Bot
import Hirc.Types

plugins :: [(T.Text, Plugin)]
plugins = [("dt", dateTime)]

dateTime :: Plugin
dateTime irc = do
    conn <- get
    time <- io getCurrentTime
    let t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" time
        in io $ writeChan (connChannel conn) $ RplPrivmsg (privDest irc) $ T.pack t
    return ()
