{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Control.Concurrent.Async qualified as Async
import Control.Monad.Except
import Data.Set qualified as Set
import Data.List qualified as L
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Options.Applicative qualified as O
import Rapid qualified
import Servant
import System.Directory
import System.IO
import Streaming.Prelude qualified as S
import Streaming qualified as S
import System.FilePath (takeExtension)

import Common.Prelude as P hiding (split)
import Corrosion qualified as C
import Server.Wai
import URL qualified
import Web hiding (port, url, root, baseUrl, split, resize)

import Orphans ()

-- * CLI

data Command
  = Web
    { root_ :: FilePath
    , thumbnailsPath_ :: Maybe FilePath
    , port :: Port
    , urlText :: TS.Text
    , verbose :: Bool
    }
  | Thumbs
    { root_ :: FilePath
    , maybeThumbDir :: Maybe FilePath
    , dryRun :: Bool
    , overwrite :: Bool
    , parallel :: Int
    }
  | ListFileTypes
    { root_ :: FilePath
    }
  deriving (Show)

mainCommand :: O.Parser Command
mainCommand = O.subparser $ mempty
  <> O.command "web" (cmd web)
  <> O.command "thumbs" (cmd thumbs)
  <> O.command "ls-types" (cmd (ListFileTypes <$> pathArgument))
  where
    cmd parser = O.info (O.helper <*> parser) O.idm

    web :: O.Parser Command
    web = Web
      <$> pathArgument
      <*> thumbDirOption
      <*> O.option O.auto (O.long "port" <> O.short 'p')
      <*> O.strOption (O.long "url")
      <*> pure False

    thumbs :: O.Parser Command
    thumbs = Thumbs
      <$> pathArgument
      <*> thumbDirOption
      <*> O.switch (O.long "dry-run" <> O.short 'n')
      <*> O.switch (O.long "overwrite")
      <*> O.option O.auto (O.long "parallel" <> O.value 16)

pathArgument :: O.Parser FilePath
pathArgument = O.argument (O.eitherReader (Right . sanitizeRoot)) rootPathField
  where
    rootPathField = O.metavar "FILE" <> O.help "Gallery root path" <> O.value "."

thumbDirOption :: O.Parser (Maybe FilePath)
thumbDirOption = O.option thumbnailsParser thumbnailsField
  where
    thumbnailsField = O.long "thumbs" <> O.short 't' <> O.help "Default is .g2llery" <> O.value Nothing
    thumbnailsParser = (O.eitherReader (\str -> Right $ Just str))

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

      dirsFiles <- liftIO $ C.lsPrim fsPath
      let dirsFiles' = map (bimap C.dropDotSlash C.dropDotSlash) dirsFiles :: [C.DirOrFilePath]
          (dirs, files') = C.partitionEithers dirsFiles'
          files = mapMaybe ensureImage files'

      let addCurrentPath p = maybe p (\currentPath -> currentPath <> "/" <> p) maybePath

      return $ do
        return $ do
          h1 $ a "Gälleri :)" ! href baseUrl
          ul $ forM_ (L.sort dirs) $ \dirName -> let
            newPath = TS.pack $ addCurrentPath dirName
            in li $ do
            a ! href (baseUrl & param "path" newPath) $ toHtml $ dirName <> "/"

          forM_ files $ \fileName -> let
            filePath = addCurrentPath fileName
            thumb = thumbUrl baseUrl filePath
            full = fullUrl baseUrl filePath
            in a ! href full $ img (pure ()) ! src thumb ! Custom "loading" "lazy"

    stub :: AppM ()
    stub = return ()

-- * Site

data Env = Env
  { root :: FilePath
  , thumbDir :: FilePath
  , baseUrl :: URL
  } deriving (Show)
type AppM = ReaderT Env Handler

app :: Env -> Wai.Application
app env = serve api $ hoistServer api (flip runReaderT env) server
  where api = Proxy @API

main :: IO ()
main = O.execParser (O.info (O.helper <*> mainCommand) O.idm) >>= \case
  Web{root_, thumbnailsPath_, port, urlText} -> do
    baseUrl <- liftEither $ either (Left . userError) Right $ URL.parse urlText
    -- todo: sanitize thumbnailsPath_
    let
      root = root_
      thumbDir = mkThumbDirRoot root thumbnailsPath_
      env = Env root thumbDir baseUrl
    print env
    C.labelPrint "port" port

    maybeTls <- tlsSettingsEnv "DEV_WEBSERVER_CERT" "WEBSERVER_KEY"
    print $ isJust maybeTls
    let settings = Warp.setPort (fromIntegral port) Warp.defaultSettings
    (maybe Warp.runSettings Warp.runTLS maybeTls) settings (app env)

  Thumbs{root_, maybeThumbDir, dryRun, overwrite, parallel} -> do

    C.cd root_
    C.pwd >>= C.labelPrint "pwd"
    C.labelPrint "root" root_
    C.labelPrint "thumbDir" thumbDir'
    C.labelPrint "dryRun" dryRun
    C.labelPrint "overwrite" overwrite

    C.lsRecursive2 root_
      & filterJpg
      & exclude ".git/"
      & if dryRun
        then S.mapM_ resize'
        else thumbsParallel parallel

    where
      thumbDir' = mkThumbDirRoot root_ maybeThumbDir :: FilePath
      resize' path = resize dryRun overwrite thumbDir' path
      thumbsParallel n source = source
        & S.chunksOf n
        & S.mapped S.toList
        & S.mapM_ (\paths -> do
                      liftIO $ Async.mapConcurrently_ resize' paths
                      mapM_ (putStrLn . ("done: " <>)) paths
                  )

  ListFileTypes{root_} -> C.lsRecursive2 root_
    & S.mapMaybe (either (\_ -> Nothing) (Just . takeExtension))
    & unique
    & S.mapM_ putStrLn

-- * Thumbnails

resize :: Bool -> Bool -> FilePath -> FilePath -> IO ()
resize dryRun overwrite thumbDir path = if dryRun
  then do
    callProcess' False mkdir
    callProcess' True convert
  else do
    callProcess_ mkdir
    exists <- doesFileExist thumbFull
    C.pwd >>= C.labelPrint "# pwd: "
    C.readShell "pwd" >>= C.labelPrint "# read shell pwd: "
    C.timePrint "convert" $ if overwrite
      then callProcess_ convert
      else if exists
           then putStrLn $ "exists: " <> thumbFull
           else callProcess_ convert
  where
    (thumbDirname, thumbFull) = thumbDirPath thumbDir path
    mkdir = ("mkdir", ["-p", thumbDirname])
    convert = (convertPath, [path, "-resize", "500x500>", thumbFull])

convertPath :: FilePath
convertPath = "convert"

mkThumbDirRoot :: FilePath -> Maybe FilePath -> FilePath
mkThumbDirRoot root maybeThumbDir = fromMaybe (root <> "/.g2llery") maybeThumbDir

thumbUrl :: URL -> FilePath -> URL
thumbUrl url relativePath = url & (segments <>~ "thumbs" : map TS.pack (split (== '/') relativePath))

fullUrl :: URL -> FilePath -> URL
fullUrl url relativePath = url & (segments <>~ "full" : map TS.pack (split (== '/') relativePath))

exclude :: FilePath -> C.Shell_ FilePath -> C.Shell_ FilePath
exclude part = S.filter (\p -> P.not $ any (part `L.isInfixOf`) $ L.tails p)

filterJpg  :: C.Shell_ C.DirOrFilePath -> C.Shell_ FilePath
filterJpg = S.mapMaybe (either (\_ -> Nothing) ensureImage)

ensureImage :: FilePath -> Maybe FilePath
ensureImage path = let
  path' = map C.toLower path
  in if ".jpg" `L.isSuffixOf` path'
  then Just path
  else Nothing

thumbDirPath :: FilePath -> FilePath -> (FilePath, FilePath)
thumbDirPath thumbRoot path = (dirPath, fullPath)
  where
    absolute = head thumbRoot == '/'
    fullPath = thumbRoot <> "/" <> path
    dirPath = (if absolute then ('/' :) else id) $ L.intercalate "/" $ init $ split (== '/') fullPath

-- * Helpers

-- ** Process

type Cmd = (FilePath, [String])

callProcess_ :: Cmd -> IO ()
callProcess_ (cmd, args) = C.callProcess cmd args

callProcess' :: MonadIO m => Bool -> Cmd -> m ()
callProcess' multiline (cmd, args) = liftIO $ putStrLn $ concat' $ cmd : args
  where
    concat' = if multiline
      then L.intercalate " \\\n\t"
      else unwords

-- ** List

split :: (Char -> Bool) -> String -> [String]
split pred = map TL.unpack . filter (P.not . TL.null) . TL.split pred . TL.pack

sanitizeRoot :: FilePath -> FilePath
sanitizeRoot = reverse . dropWhile (== '/') . reverse

-- ** Streaming

unique :: S.Stream (S.Of FilePath) IO () -> S.Stream (S.Of FilePath) IO ()
unique = go mempty
  where
    go seen source = lift (S.next source) >>= \case
      Left r -> return r
      Right (path, source') -> if Set.member path seen
        then go seen source'
        else S.yield path *> go (Set.insert path seen) source'

-- * Hot reload

hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
