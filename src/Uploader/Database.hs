{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Uploader.Database where

import Uploader.Types
import Uploader.Util

import Platform.SQLite
import Data.Maybe
import Data.Int
import Data.List.Split
import System.FilePath.Posix (splitExtension)
import qualified Data.Text as T

import Database.SQLite.Simple

insertFile :: DB r m => UploadFile -> m ()
insertFile form = withConn $ \conn ->
  execute conn qry
    ( uploadFileName form
    , uploadFileType form
    , uploadFileSize form
    , uploadFileURI form
    , uploadFileIsLink form )

    where
      qry = "INSERT INTO files (name, type, size, uri, is_link, created_at) \
            \ VALUES (?, ?, ?, ?, ?, datetime('now'))"

deleteFile :: DB r m => String -> m ()
deleteFile fileId = withConn $ \conn ->
  execute conn qry (Only fileId)
    where
      qry = "DELETE FROM files WHERE name = ?"

isFileExist :: DB r m => String -> m Bool
isFileExist name =  withConn $ \conn -> do
  results <- query conn qry (Only name) :: IO [Only Int]
  return $ not (null results)
  where
    qry = "SELECT 1 FROM files WHERE name = ?"

findFile :: DB r m => String -> m (Maybe UploadFile)
findFile name = do
  results <- withConn $ \conn -> query conn qry (Only name)
  return $ listToMaybe results
  where
    qry = "SELECT name, type, size, uri, is_link as created FROM files WHERE name = ?"

findRealFiles :: DB r m => String -> Int64 -> m [UploadFile]
findRealFiles contentType size =
  withConn $ \conn -> query conn qry (contentType, size)
  where
    qry = "SELECT name, type, size, uri, is_link as created FROM files WHERE type = ? AND size = ?"

findDuplicatedFileNames :: DB r m => String -> m [String]
findDuplicatedFileNames fileName =
  withConn $ \conn -> do
    results <- query conn qry (Only nameValue) :: IO [Only String]
    return $ map (\(Only x) -> x) results
    where
      qry = "SELECT name FROM files WHERE name LIKE ?"
      (name, ext) = splitExtension fileName
      nameValue = name ++ "%" ++ ext

findLinkFiles :: DB r m => String -> Int -> m [UploadFile]
findLinkFiles uri limit =
  withConn $ \conn -> query conn qry (uri, limit)
    where
      qry = "SELECT name, type, size, uri, is_link as created FROM files WHERE uri = ? LIMIT ?"

lockFile :: DB r m => String -> m ()
lockFile name =
  withConn $ \conn -> execute conn qry (Only name)
    where
      qry = "UPDATE files SET is_link = 1 WHERE name = ?"
