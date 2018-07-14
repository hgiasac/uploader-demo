{-# LANGUAGE OverloadedStrings #-}
module Uploader.Service where

import Data.Int
import Data.List
import Data.Maybe
import Network.Wai.Parse
import Control.Monad.IO.Class
import Control.Monad.Except

import qualified Data.Text as T

import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import System.FilePath
import Debug.Trace
import Uploader.Types
import Uploader.Util

-- Data Access class
class (Monad m) => UploadFileRepo m where
  insertFile :: UploadFile -> m ()
  findFileByName :: String -> m (Maybe UploadFile)
  findRealFiles :: String -> Int64 -> m [UploadFile]
  deleteFileByName :: String -> m ()
  findDuplicatedFileNames :: String -> m [String]
  findLinkFiles :: String -> Int -> m [UploadFile]
  lockFile :: String -> m ()

-- File Handler class
class (MonadIO m) => UploadFileHandler m where
  saveFileToDisk :: FilePath -> FileInfo B.ByteString -> m UploadFile
  deleteFileFromDisk :: FilePath -> m ()
  isSameFile :: FilePath -> B.ByteString -> m Bool

-- File Service Implementation

createFile :: (UploadFileRepo m, UploadFileHandler m)
  => String -> FileInfo B.ByteString -> m (Either UploadFileError UploadFile)
createFile filePath f = do
  sameFile <- checkSameFile filePath f
  case sameFile of
    Nothing -> savePhysicFile filePath f
    Just file -> saveLinkFile fName file

  where
    fName = BS.unpack $ fileName f

getFile :: (UploadFileRepo m)
  => String -> m (Either UploadFileError UploadFile)
getFile name = runExceptT $ do
  result <- lift $ findFileByName name
  case result of
    Nothing -> throwError $ UploadFileNotFound (LT.pack name)
    Just model -> return model

-- delete file in both database and disk
deleteFile :: (UploadFileRepo m, UploadFileHandler m)
  => String -> Maybe LT.Text -> m (Either UploadFileError ())
deleteFile name force = do
  model <- findFileByName name
  case model of
    Nothing -> return $ Left (UploadFileNotFound "File not found!")
    Just model -> if uploadFileIsLink model > 0 && not forceDel
                  -- The file is locked. Can not be deleted
                  then return $ Left (UploadFileBadRequest "The file is used by another user")
                  -- Delete from database and remove from disk if there isn't any file linked to it
                  else return (Right ()) <* clearFile model
    where
      forceDel = force /= Nothing && force /= (Just "")

-- Calculate Unique File Name from Database
getUniqueFileName :: (UploadFileRepo m) => FilePath -> m String
getUniqueFileName name = do
  ns <- findDuplicatedFileNames name
  return $ calculateUniqueFileName name ns


-- Check if multiple files have similar contents, reuse the contents somehow to save space.
checkSameFile ::  (UploadFileRepo m, UploadFileHandler m)
                   => FilePath
                   -> FileInfo B.ByteString
                   -> m (Maybe UploadFile)
checkSameFile filePath f = do
  files' <- findRealFiles (uploadFileType cf) (uploadFileSize cf)

  let sameFiles = filterSameFiles cf files'

  sameContentFiles <- filterM (\fx -> isSameFile (uploadFileURI fx) fc) sameFiles

  return $ if null sameContentFiles
           then Nothing
           else Just $ fromMaybe (head sameContentFiles)
                     $ find (\fx -> uploadFileName cf == uploadFileName fx) sameContentFiles
  where
    cf = extractFileInfo filePath f 0
    fc = fileContent f

-- Save link file,
-- returns error if file name is same but the content is different
saveLinkFile :: (UploadFileRepo m, UploadFileHandler m)
                => String
                -> UploadFile
                -> m (Either UploadFileError UploadFile)
saveLinkFile fName file
  | fName == uploadFileName file = (return $ Right file) <* lockFile fName
  | otherwise = do
      sameNameFile <- findFileByName fName
      case sameNameFile of
        Just f -> if uploadFileURI file == uploadFileURI f
                     then return (Right f) <* lockFile fName
                     else return $ (Left (UploadFileBadRequest "The file is existed"))
        Nothing -> (return . const (Right model))  =<< insertFile model
   where
     model = UploadFile
              { uploadFileName = fName
              , uploadFileSize = uploadFileSize file
              , uploadFileType = uploadFileType file
              , uploadFileURI = uploadFileURI file
              , uploadFileIsLink = 0 }

-- Save physic file into database and storage
-- Returns error if file name is existed, but content is different
savePhysicFile :: (UploadFileRepo m, UploadFileHandler m)
                  => FilePath
                  -> FileInfo B.ByteString
                  -> m (Either UploadFileError UploadFile)
savePhysicFile fPath file = do
  sameFileName <- findFileByName $ BS.unpack (fileName file)
  case sameFileName of
    Just f -> return $ Left (UploadFileBadRequest "The file is existed")
    Nothing -> do
      savedModel <- saveFileToDisk fPath file
      insertFile savedModel
      return $ Right savedModel

clearFile :: (UploadFileRepo m, UploadFileHandler m)
             => UploadFile
             -> m (Either UploadFileError ())
clearFile model = do
  deleteFileByName $ uploadFileName model
  sameFiles <- findLinkFiles (uploadFileURI model) 1
  if null sameFiles
           then return $ Right ()
           else (return $ Right ()) <* (deleteFileFromDisk $ uploadFileURI model)
