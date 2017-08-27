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
        Post, Raw, ReqBody, Server, serve, serveDirectoryWebApp)
import Servant.HTML.Blaze (HTML)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown
       (def, htmlFencedHandler, markdown, msFencedHandlers, msBlockFilter, Block(..), Inline(..))

import Protolude

type Api
   = "uploadmd" :> ReqBody '[ Markdown, PlainText] LText :> Post '[ HTML] Html :<|> Raw

data Markdown

instance Accept Markdown where
  contentType _ = "text" // "markdown" /: ("charset", "utf-8")

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
server = mdConverter :<|> rawServer

rawServer = serveDirectoryWebApp "./html/"

mdConverter = pure . markdown mdSettings

mdSettings = def {msFencedHandlers = modifiedFencedHandlers, msBlockFilter = map codeModifier}
  where
    modifiedFencedHandlers =
      htmlFencedHandler
        "```"
        (\lang ->
           "<pre class=\"lang:" <> lang <>
           " theme:intellij-idea decode:true nums:false striped:false toolbar:false\"")
        (const "</pre>")
    codeModifier :: Block [Inline] -> Block [Inline]
    codeModifier (BlockPara inlines) = BlockPara $ map changeInlineCode inlines
    codeModifier (BlockList orderType listContent) = BlockList orderType $ eitherDimap (map changeInlineCode) (map codeModifier) listContent
    codeModifier (BlockQuote blocks) = BlockQuote (map codeModifier blocks)
    codeModifier (BlockHeading level inlines) = BlockHeading level (map changeInlineCode inlines)
    codeModifier (BlockPlainText inlines) = BlockPlainText (map changeInlineCode inlines)
    codeModifier rest = rest

    changeInlineCode (InlineCode code) = InlineHtml $ "<span class=\"crayon-inline lang:java theme:intellij-idea decode:true\">" <> code <> "</span>"
    changeInlineCode rest = rest


eitherDimap :: (e -> e1) -> (a -> a1) -> Either e a -> Either e1 a1
eitherDimap _ f (Right a) = Right (f a)
eitherDimap f _ (Left e) = Left (f e)
