module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Rapid
import System.IO

import X.Prelude as P
import X
import URL.TH
import Warp_Helpers (tlsSettingsEnvIO)

-- * Site

data RunConf = RunConf { runConfDynPath :: [Segment] }
makeFields ''RunConf

instance Default RunConf where
  def = RunConf mempty

site :: T RunConf
site = T $ do
  return $ \_ -> mdo
    return $ htmlDoc "" "hello world"

-- * Web server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  maybeTls <- tlsSettingsEnvIO "DEV_WEBSERVER_CERT" "DEV_WEBSERVER_KEY"
  siteMain maybeTls def def prodUrl settings site
  where settings = Warp.setPort 8082 Warp.defaultSettings

prodUrl :: URL
prodUrl = [url| https://fixme.com/ |]

-- * Hot reload

hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
