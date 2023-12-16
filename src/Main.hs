{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Data.List qualified as L
import Data.Text qualified as TS
import Data.Text.Lazy.Encoding qualified as TL
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai qualified as Wai
import Rapid qualified
import System.IO
import System.Directory
import Network.HTTP.Media ((//), (/:))
import Control.Monad.Except

import Options.Applicative qualified as O

import Common.Prelude as P
-- import X hiding (path, contents)
-- import X.Template.V2
-- import URL.TH
-- import Warp_Helpers (tlsSettingsEnvIO)

import Server.Wai

import GHC.Generics (Generic)
import Data.Aeson qualified as A
import Servant.Server
import Servant

import CSS qualified
import JS qualified
import HTML qualified
import Web.DSL qualified
import Web hiding (port, url, root, baseUrl)
import URL qualified

import Corrosion qualified as C
import Streaming.Prelude qualified as S


-- * CLI

data Options = Options
  { rootPath :: FilePath
  , port :: Port
  , urlText :: TS.Text
  , verbose :: Bool
  } deriving (Show)

opts :: O.Parser Options
opts = Options
  <$> O.argument O.str (O.metavar "FILE" <> O.help "Gallery root path" <> O.value ".")
  <*> O.option O.auto (O.long "port" <> O.short 'p')
  <*> O.strOption (O.long "url")
  <*> pure False

-- * API

type API
  = QueryParam "path" FilePath :> Get '[Html] (Web Html)
  :<|> "stub" :> Get '[JSON] ()

server :: ServerT API AppM
server = browse :<|> stub
  where
    browse :: Maybe FilePath -> AppM (Web Html)
    browse maybePath = do
      Env{root, baseUrl} <- ask

      let fsPath = maybe root (\p -> root <> "/" <> p) maybePath

      dirsFiles <- liftIO $ S.toList_ $ C.ls' fsPath
      let dirsFiles' = map (bimap dropDotSlash dropDotSlash) dirsFiles :: [C.DirOrFilePath]

      return $ do
        return $ do
          h1 "Gälleri :)"
          ul $ forM_ dirsFiles' $ \case
            Left dirName -> let
              newPath = TS.pack dirName
              in li $ do
              a ! href (baseUrl & param "path" newPath) $ toHtml $ dirName <> "/"
              toHtml "[dir]"
            Right file -> li $ a ! HTML.href "" $ toHtml $ file

    stub :: AppM ()
    stub = return ()

instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Html (Web Html) where
  mimeRender _ web = TL.encodeUtf8 $ render' $ runStatic web

-- * Site

data Env = Env
  { root :: FilePath
  , baseUrl :: URL
  }
type AppM = ReaderT Env Handler

app :: Env -> Wai.Application
app env = serve api $ hoistServer api (flip runReaderT env) server
  where api = Proxy @API

main :: IO ()
main = do
  Options{rootPath, port, urlText, verbose} <- O.execParser (O.info opts O.idm)
  baseUrl <- liftEither $ either (Left . userError) Right $ URL.parse urlText
  print baseUrl
  maybeTls <- tlsSettingsEnv "DEV_WEBSERVER_CERT" "WEBSERVER_KEY"
  let settings = Warp.setPort (fromIntegral port) Warp.defaultSettings
  (maybe Warp.runSettings Warp.runTLS maybeTls) settings (app $ Env rootPath baseUrl)

-- * Helpers

-- | Drop "./" from a string
dropDotSlash :: String -> String
dropDotSlash xs = fromMaybe xs $ L.stripPrefix "./" xs

-- * Hot reload

hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
