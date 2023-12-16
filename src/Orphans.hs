{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans () where

import Prelude
import Network.HTTP.Media ((//), (/:))
import Servant
import Data.Text.Lazy.Encoding qualified as TL
import Web (Web, Html, runStatic, render')

instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Html (Web Html) where
  mimeRender _ web = TL.encodeUtf8 $ render' $ runStatic web
