{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Extension where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           System.FilePath
import           System.Directory
import           Control.Monad

type URL = String

data Defaults = Defaults {
  defaultsUser :: String
, defaultsRef :: String
} deriving (Eq, Show)

parseDefaults :: String -> Maybe Defaults
parseDefaults input =  case break (== '-') $ reverse input of
  (reverse -> ref@(_:_), '-' : (reverse -> user@(_:_))) -> Just (Defaults user ref)
  _ -> Nothing

defaultsUrl :: Defaults -> URL
defaultsUrl Defaults{..} = "https://raw.githubusercontent.com/" ++ defaultsUser ++ "/hpack-template/" ++ defaultsRef ++ "/defaults.yaml"

defaultsPath :: FilePath -> Defaults -> FilePath
defaultsPath dir Defaults{..} = dir </> "defaults" </> defaultsUser ++ "-" ++ defaultsRef

get :: URL -> IO ByteString
get url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    -- FIXME: check status code == 200
    responseBody <$> httpLbs request manager

ensure :: FilePath -> Defaults -> IO ()
ensure dir defaults = ensureFile file url
  where
    url = defaultsUrl defaults
    file = defaultsPath dir defaults

ensureFile :: FilePath -> URL -> IO ()
ensureFile file url = do
  createDirectoryIfMissing True (takeDirectory file)
  exists <- doesFileExist file
  unless exists $ do
    get url >>= B.writeFile file
