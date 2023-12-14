{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Rapid qualified
import System.IO
import System.Directory

import Options.Applicative qualified as O

import Common.Prelude as P
-- import X hiding (path, contents)
-- import X.Template.V2
import URL
import URL.TH
-- import Warp_Helpers (tlsSettingsEnvIO)

import Server.Wai

import Servant.Server
import Servant

-- * CLI

data Options = Options
  { rootPath :: FilePath
  , url :: String
  , verbose :: Bool
  } deriving (Show)

opts :: O.Parser Options
opts = Options
  <$> O.argument O.str (O.metavar "FILE" <> O.help "Gallery root path" <> O.value ".")
  <*> O.strOption (O.long "url")
  <*> pure False

-- * Site

main :: IO ()
main = do
  opts :: Options <- O.execParser (O.info opts O.idm)
  maybeTls <- tlsSettingsEnv "DEV_WEBSERVER_CERT" "DEV_WEBSERVER_KEY"
  return ()
--  siteMain maybeTls def def prodUrl settings $ site opts
  where settings = Warp.setPort 8082 Warp.defaultSettings

-- data RunConf = RunConf { runConfDynPath :: [Segment] }
-- makeFields ''RunConf

-- instance Default RunConf where
--   def = RunConf mempty

-- data FolderListing = FolderListing
--   { folderListingPath :: FilePath
--   , folderListingContents :: [FilePath]
--   }
-- makeFields ''FolderListing

-- getFolderListing :: FilePath -> IO FolderListing
-- getFolderListing path = do
--   list <- listDirectory path
--   return $ FolderListing path list

-- site :: Options -> T RunConf
-- site opts = T $ do
--   return $ \_ -> do
--     fl <- liftIO $ getFolderListing $ rootPath opts
--     return $ htmlDoc "" $ do
--       h1 $ fl^.path.html
--       ul $ forM_ (fl^.contents) $ do
--         li . toHtml

-- * Web server

-- prodUrl :: URL
-- prodUrl = [url| https://fixme.com/ |]

-- -- * Hot reload

-- hot, stop :: IO ()
-- hot = Rapid.rapid 0 $ \r -> do
--   Rapid.restart r ("webserver" :: String) main
-- stop = Rapid.rapid 0 $ \r -> do
--   Rapid.stop r ("webserver" :: String) main
