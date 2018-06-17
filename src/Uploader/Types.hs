module Uploader.Types where

import Data.Text
import Data.Time
import Database.SQLite.Simple.FromRow


data CreateUploadFile = CreateUploadFile
  { createUploadFileName :: Text
  , createUploadFileType :: Text
  , createUploadFileSize :: Integer
  , createUploadFileURI :: Text
  } deriving (Eq, Show)

data UploadFile = UploadFile
  { uploadFileName :: Text
  , uploadFileType :: Text
  , uploadFileSize :: Integer
  , uploadFileURI :: Text
  , uploadFileCreatedAt :: UTCTime
  } deriving (Show, Eq)

data UploadFileError
  = UploadFileNotFound Text
  | UploadFileBadRequest Text
  deriving (Eq, Show)

instance FromRow UploadFile where
  fromRow = UploadFile
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
