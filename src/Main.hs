{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Data.Text.Lazy.Encoding qualified as TL
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai qualified as Wai
import Rapid qualified
import System.IO
import System.Directory
import Network.HTTP.Media ((//), (/:))

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
import Web hiding (port, url)

-- * CLI

data Options = Options
  { rootPath :: FilePath
  , port :: Port
  , url :: String
  , verbose :: Bool
  } deriving (Show)

opts :: O.Parser Options
opts = Options
  <$> O.argument O.str (O.metavar "FILE" <> O.help "Gallery root path" <> O.value ".")
  <*> O.option O.auto (O.long "port" <> O.short 'p')
  <*> O.strOption (O.long "url")
  <*> pure False

-- * API

type Site
  = Get '[Html] (Web Html)
  :<|> "api" :> "browse" :> QueryParam "path" FilePath :> Get '[JSON] [Either FilePath FilePath]
  :<|> "stub" :> Get '[JSON] ()

app :: Server Site
app = home :<|> browse :<|> stub
  where
    home :: Handler (Web Html)
    home = return $ do
      return $ do
        h1 "Gälleri :)"

    browse :: Maybe FilePath -> Handler [Either FilePath FilePath]
    browse maybePath = return []

    stub :: Handler ()
    stub = return ()

instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Html (Web Html) where
  mimeRender _ web = TL.encodeUtf8 $ render' $ runStatic web

-- * Site

site :: Wai.Application
site = serve (Proxy @Site) app

main :: IO ()
main = do
  Options{rootPath, port, url, verbose} <- O.execParser (O.info opts O.idm)
  maybeTls <- tlsSettingsEnv "DEV_WEBSERVER_CERT" "WEBSERVER_KEY"
  let settings = Warp.setPort (fromIntegral port) Warp.defaultSettings
  (maybe Warp.runSettings Warp.runTLS maybeTls) settings site

data FolderListing = FolderListing
  { folderListingPath :: FilePath
  , folderListingContents :: [FilePath]
  }

getFolderListing :: FilePath -> IO FolderListing
getFolderListing path = do
  list <- listDirectory path
  return $ FolderListing path list

-- * Hot reload

hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
