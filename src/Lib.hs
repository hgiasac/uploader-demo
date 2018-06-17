{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
  (server) where

import Web.Scotty

import Control.Monad.IO.Class
import Data.Monoid

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Network.Wai.Middleware.Cors

import Control.Monad.Reader
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BSb
import System.FilePath ((</>))

import qualified Platform.SQLite as DB
import qualified Platform.HTTP as HTTP

type Env = DB.Env

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving  ( Applicative, Functor, Monad
              , MonadIO, MonadReader Env)

server :: IO ()
server = do
  -- acquire resources
  dbEnv <- DB.init
  -- start the app
  let runner app = flip runReaderT dbEnv $ unAppT app
  HTTP.main runner
