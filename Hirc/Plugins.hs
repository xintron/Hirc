{-# LANGUAGE OverloadedStrings #-}
module Hirc.Plugins where

import Control.Concurrent.Chan
import Control.Monad.State
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Database.SQLite.Simple
import System.Locale

import Hirc.Bot
import Hirc.Types

plugins :: [(T.Text, Plugin)]
plugins = [("dt", dateTime)
           , ("seen", seen)]

dateTime :: Plugin
dateTime irc = do
    conn <- get
    time <- io $ getCurrentTime >>= utcToLocalZonedTime
    let t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" time
        in io $ writeChan (connChannel conn) $ RplPrivmsg (privDest irc) $ T.pack t
    return ()

seen :: Plugin
seen (Privmsg { privDest = d, privText = c }) = do
    conn <- get
    let db = connDb conn
        ch = servName (connServer conn) <> d
    io $ when (length args > 1) $ do
        xs <- query db
            "SELECT nick, message, timestamp FROM seen WHERE \
                \channel = ? AND nick = ? COLLATE NOCASE"
                    (ch, args !! 1) :: IO [(Text,Text,UTCTime)]
        if null xs
            then writeChan (connChannel conn) $
                RplPrivmsg d $ "I have not seen " <> args !! 1
            else forM_ xs $ \(n,msg,st) -> do
                tzone <- utcToLocalZonedTime st
                writeChan (connChannel conn) $
                    RplPrivmsg d $
                        n <> " was last seen: " <> time tzone <> " :" <> msg
  where
    time = T.pack . formatTime defaultTimeLocale "%Y-%M-%d %H:%M:%S %Z"
    args :: [Text]
    args = T.words c
