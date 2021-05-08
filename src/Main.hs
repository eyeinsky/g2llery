module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Rapid
import System.IO
import System.Directory

import Options.Applicative as O

import X.Prelude as P
import X hiding (path, contents)
import X.Template.V2
import URL
import URL.TH
import Warp_Helpers (tlsSettingsEnvIO)

-- * CLI

data Options = Options
  { rootPath :: FilePath
  , verbose :: Bool
  } deriving (Show)

opts :: Parser Options
opts = Options <$> path' <*> pure False
  where
    path' = O.argument O.str
      $ metavar "FILE"
      <> help "Gallery root path"
      <> value "."

-- * Site

data RunConf = RunConf { runConfDynPath :: [Segment] }
makeFields ''RunConf

instance Default RunConf where
  def = RunConf mempty

data FolderListing = FolderListing
  { folderListingPath :: FilePath
  , folderListingContents :: [FilePath]
  }
makeFields ''FolderListing

getFolderListing :: FilePath -> IO FolderListing
getFolderListing path = do
  list <- listDirectory path
  return $ FolderListing path list

site :: Options -> T RunConf
site opts = T $ do
  return $ \_ -> mdo
    fl <- liftIO $ getFolderListing $ rootPath opts
    return $ htmlDoc "" $ do
      h1 $ fl^.path.html
      ul $ forM_ (fl^.contents) $ do
        li . toHtml

-- * Web server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts :: Options <- execParser (info opts idm)
  maybeTls <- tlsSettingsEnvIO "DEV_WEBSERVER_CERT" "DEV_WEBSERVER_KEY"
  siteMain maybeTls def def prodUrl settings $ site opts
  where settings = Warp.setPort 8082 Warp.defaultSettings

prodUrl :: URL
prodUrl = [url| https://fixme.com/ |]

-- * Hot reload

hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
