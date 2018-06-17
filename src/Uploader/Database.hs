{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Uploader.Database where

import Uploader.Types (CreateUploadFile(..), UploadFile)
import Platform.SQLite
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple

insertFile :: DB r m => CreateUploadFile -> m ()
insertFile form = withConn $ \conn ->
  execute conn qry
    ( createUploadFileName form
    , createUploadFileType form
    , createUploadFileSize form
    , createUploadFileURI form)

    where
      qry = "INSERT INTO files (name, type, size, uri, created_at) \
            \ VALUES (?, ?, ?, ?, datetime('now'))"

deleteFile :: DB r m => T.Text -> m ()
deleteFile fileId = withConn $ \conn ->
  execute conn qry (Only fileId)
    where
      qry = "DELETE FROM files WHERE name = ?"

isFileExist :: DB r m => T.Text -> m Int
isFileExist name =  withConn $ \conn -> do
  results <- query conn qry (Only name) :: IO [Only Int]
  return $ length results
  where
    qry = "SELECT 1 FROM files WHERE name = ?"

findFile :: DB r m => T.Text -> m (Maybe UploadFile)
findFile name = do
  results <- withConn $ \conn -> query conn qry (Only name)
  return $ listToMaybe results
  where
    qry = "SELECT name, type, size, uri, strftime('%Y-%m-%dT%H:%M:%fZ', created_at) as created FROM files WHERE name = ?"
