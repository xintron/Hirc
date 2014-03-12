{-# LANGUAGE OverloadedStrings #-}
module Hirc.Hooks where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import Database.SQLite.Simple
import Network.HTTP.Conduit
import Text.HTML.TagSoup

import Hirc.Bot
import Hirc.Types

hooks :: [Hook]
hooks = [seen]

seen :: Hook
seen (Privmsg n _ _ d t) = do
    conn <- get
    let db = connDb conn
        name = servName $ connServer conn
    io $ execute db "INSERT OR REPLACE INTO seen \
        \(channel, nick, message, timestamp) \
        \VALUES (?, ?, ?, datetime('now'))" (name <> d, n, t)

http :: Hook
http (Privmsg {privDest = d, privText = text}) = do
    chan <- liftM connChannel get
    io $ forM_ (urls text) $ forkIO . fetchTitle chan
  where
    urls :: Text -> [Text]
    urls c = [x | x <- T.words c,
        "http://" == T.toLower (T.take 7 x) || "https://" == T.toLower (T.take 8 x)]
    fetchTitle :: Chan IRCRpl -> Text -> IO ()
    fetchTitle chan url = do
        req <- parseUrl $ T.unpack url
        let req' = req { requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:27.0) Gecko/20100101 Firefox/27.0")] }
        manager <- newManager conduitManagerSettings
        resp <- httpLbs req' manager
        case title (responseBody resp) of
            Just t -> writeChan chan $ RplPrivmsg d $ decodeUtf8 (toStrict t)
            Nothing -> return ()
    title:: ByteString -> Maybe ByteString
    title = maybeTagText . head . drop 1 . dropWhile (not . isTagOpenName "title") . parseTags
