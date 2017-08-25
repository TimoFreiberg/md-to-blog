{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
       (defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant ((:>), PlainText, Post, ReqBody, Server, serve)
import System.Environment (getEnv)

import Protolude

type Api = "md" :> ReqBody '[ PlainText] Text :> Post '[ PlainText] Text

type Port = Int

main :: IO ()
main = do
  portString <- getEnv "PORT"
  port <-
    case readMaybe @Int portString of
      Just someInt -> pure someInt
      Nothing ->
        error $
        "Environment variable PORT must be an int, but was: " <>
        strConv Lenient portString
  startServer port

startServer :: Port -> IO ()
startServer port = do
  putText $ "Starting server on port " <> show port
  let settings = setPort port defaultSettings
  runSettings settings $ logStdoutDev (serve (Proxy @Api) server)

server :: Server Api
server = pure
