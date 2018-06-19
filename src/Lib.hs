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

import qualified Uploader.HTTP as UploaderHTTP
import qualified Uploader.Service as UploaderService
import qualified Uploader.File as UploaderFile
import qualified Uploader.Database as UploaderDB

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


instance UploaderHTTP.Service AppT where
  createFile = UploaderService.createFile
  getFile = UploaderService.getFile
  deleteFile = UploaderService.deleteFile


-- File Handler class
instance UploaderService.UploadFileHandler AppT where
  saveFileToDisk = UploaderFile.saveFile
  deleteFileFromDisk = UploaderFile.deleteFile
  isSameFile = UploaderFile.isSameFile

instance UploaderService.UploadFileRepo AppT where
  insertFile = UploaderDB.insertFile
  findFileByName = UploaderDB.findFile
  deleteFileByName = UploaderDB.deleteFile
  findRealFiles = UploaderDB.findRealFiles
  findDuplicatedFileNames = UploaderDB.findDuplicatedFileNames
  findLinkFiles = UploaderDB.findLinkFiles
  changeLinkFileToReal = UploaderDB.changeLinkFileToReal
