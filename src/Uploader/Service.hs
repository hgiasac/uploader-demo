module Uploader.Service where

import Data.Int
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
  insertFile :: CreateUploadFile -> m ()
  findFileByName :: String -> m (Maybe UploadFile)
  findRealFiles :: String -> Int64 -> m [UploadFile]
  deleteFileByName :: String -> m ()
  findDuplicatedFileNames :: String -> m [String]
  findLinkFiles :: String -> Int -> m [UploadFile]
  changeLinkFileToReal :: String -> m ()

-- File Handler class
class (MonadIO m) => UploadFileHandler m where
  saveFileToDisk :: FilePath -> FileInfo B.ByteString -> m CreateUploadFile
  deleteFileFromDisk :: FilePath -> m ()
  isSameFile :: FilePath -> B.ByteString -> m Bool

-- File Service Implementation

createFile :: (UploadFileRepo m, UploadFileHandler m)
  => String -> FileInfo B.ByteString -> m (Either UploadFileError CreateUploadFile)
createFile filePath f = do
  sameFile <- checkSameFile filePath f
  cf <- maybe (saveFileToDisk filePath f) return sameFile

  fName <- getUniqueFileName $ createUploadFileName cf
  let model = trace (fName ++ " " ++ show cf) $ if fName == createUploadFileName cf
        then cf
        else CreateUploadFile
              { createUploadFileName = fName
              , createUploadFileSize = createUploadFileSize cf
              , createUploadFileType = createUploadFileType cf
              , createUploadFileURI = createUploadFileURI cf
              , createUploadFileIsLink = createUploadFileIsLink cf }

  insertFile model
  return $ Right model

getFile :: (UploadFileRepo m)
  => String -> m (Either UploadFileError UploadFile)
getFile name = runExceptT $ do
  result <- lift $ findFileByName name
  case result of
    Nothing -> throwError $ UploadFileNotFound (LT.pack name)
    Just model -> return model

-- delete file in both database and disk
deleteFile :: (UploadFileRepo m, UploadFileHandler m)
  => String -> m (Either UploadFileError ())
deleteFile name = do
  result <- findFileByName name
  case result of
    Nothing -> return $ Right ()
    Just model -> deleteFile' model
      where
        deleteFile' mdl
          -- if record is link file, just delete from database
          | uploadFileIsLink model == 1 = do
              deleteFileByName name
              return $ Right ()
          -- otherwise, check there is any shortcut file.
          -- If exists, just change link status to real
          | otherwise = do
              linkFiles <- findLinkFiles (uploadFileURI model) 1
              case linkFiles of
                [] -> deleteFileFromDisk $ uploadFileURI model
                [lf] -> changeLinkFileToReal $ uploadFileName lf
              deleteFileByName name
              return $ Right ()


-- Calcualte Unique File Name from Database
getUniqueFileName :: (UploadFileRepo m) => FilePath -> m String
getUniqueFileName name = do
  ns <- findDuplicatedFileNames name
  return $ calculateUniqueFileName name ns


-- Check if multiple files have similar contents, reuse the contents somehow to save space.
checkSameFile ::  (UploadFileRepo m, UploadFileHandler m) => FilePath -> FileInfo B.ByteString -> m (Maybe CreateUploadFile)
checkSameFile filePath f = do
  files' <- findRealFiles (createUploadFileType cf) (createUploadFileSize cf)

  let sameFiles = filterSameFiles cf files'

  sameContentFiles <- filterM (\fx -> isSameFile (uploadFileURI fx) fc) sameFiles
  if not (null sameContentFiles)
    then return $ Just CreateUploadFile
                  { createUploadFileName = createUploadFileName cf
                  , createUploadFileSize = createUploadFileSize cf
                  , createUploadFileType = createUploadFileType cf
                  , createUploadFileURI = uploadFileURI $ head sameContentFiles
                  , createUploadFileIsLink = 1 }
    else return Nothing
  where
    cf = extractFileInfo filePath f 0
    fc = fileContent f
