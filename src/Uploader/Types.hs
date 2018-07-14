{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Uploader.Types where

import Data.Text
import Data.Int
import Data.Time
import Data.Aeson
import Database.SQLite.Simple.FromRow
import qualified Data.Text.Lazy as LT
import GHC.Generics

data UploadFile = UploadFile
  { uploadFileName :: FilePath
  , uploadFileType :: String
  , uploadFileSize :: Int64
  , uploadFileURI :: FilePath
  , uploadFileIsLink :: Int
  -- , uploadFileCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON UploadFile where
  toJSON m =
    object ["name"  .= uploadFileName m
       , "type" .= uploadFileType m
       , "size" .= uploadFileSize m
       , "uri"  .= uploadFileURI m
       , "isLink" .= uploadFileIsLink m]


data UploadFileError
  = UploadFileNotFound LT.Text
  | UploadFileBadRequest LT.Text
  deriving (Eq, Show)

instance FromRow UploadFile where
  fromRow = UploadFile
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    -- <*> field

newtype ErrorWrapper = ErrorWrapper
  { message :: LT.Text }
  deriving (Eq, Show, Generic)

instance ToJSON ErrorWrapper
