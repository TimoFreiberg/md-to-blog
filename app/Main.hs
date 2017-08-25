{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Arrow (left)

import Control.Monad.Catch (throwM)
import qualified Data.Text.Lazy.Encoding as LText (decodeUtf8')
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
       ((:<|>)((:<|>)), (:>), Accept(contentType), Get, Handler, JSON,
        MimeRender(mimeRender), MimeUnrender(mimeUnrender), PlainText,
        Post, ReqBody, Server, serve)
import System.Environment (getEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (def, markdown)

import Protolude

type Api
   = "md" :> ReqBody '[ Markdown, PlainText] LText :> Post '[ Html, PlainText] LText :<|> "uploadmd" :> Get '[ PlainText] LText

data Html

data Markdown

instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance Accept Markdown where
  contentType _ = "text" // "markdown" /: ("charset", "utf-8")

instance MimeRender Html LText where
  mimeRender _ = strConv Lenient

instance MimeUnrender Markdown LText where
  mimeUnrender _ = left show . LText.decodeUtf8'

type Port = Int

data ConfigException =
  ConfigException Text
  deriving (Show)

instance Exception ConfigException

main :: IO ()
main = do
  portString <- getEnv "PORT"
  port <-
    case readMaybe @Int portString of
      Just someInt -> pure someInt
      Nothing ->
        throwM . ConfigException $
        "Environment variable PORT must be an int, but was: " <>
        strConv Lenient portString
  startServer port

startServer :: Port -> IO ()
startServer port = do
  putText $ "Starting server on port " <> show port
  run port $ logStdoutDev (serve (Proxy @Api) server)

server :: Server Api
server = mdConverter :<|> mdUploader

mdUploader = pure . renderHtml $ markdown def "# Upload Markdown"

mdConverter = pure . renderHtml . markdown def
