module Uploader.File where

import Network.Wai.Parse
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Environment

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import System.FilePath ((</>))
import System.Directory (doesFileExist, removeFile)

import Uploader.Types
import Uploader.Util

-- Get base upload directory from environment
uploadBasePath :: IO String
uploadBasePath = (\x -> if x == "" then "uploads"
              else x) . fromMaybe "" <$> lookupEnv "UPLOAD_DIR"

-- Change file name surfix to avoid override duplicated file name
correctFileName :: FilePath -> FilePath -> IO FilePath
correctFileName filePath name =
  correctFileName' 0 filePath name
  where
    correctFileName' num filePath' name' =
      doesFileExist (filePath' </> fName) >>= \doesExist -> if not doesExist
        then return fName
        else correctFileName' (num + 1) filePath' name'

      where
        fName = if num == 0
                then name
                else appendFileNameSurfix name (show num)


-- Save file into hard disk
saveFile :: MonadIO m => FilePath -> FileInfo B.ByteString -> m CreateUploadFile
saveFile uploadPath file = do
  fName <- liftIO $ correctFileName uploadPath $ BS.unpack (fileName file)
  liftIO $ B.writeFile (uploadPath </> fName) fc
  return $ CreateUploadFile
    fName
    (BS.unpack $ fileContentType file)
    (B.length fc)
    (uploadPath </> fName)
    0
  where
    fc = fileContent file

-- Delete file from path
deleteFile :: MonadIO m => FilePath -> m ()
deleteFile filePath = liftIO $ doesFileExist filePath
  >>= \doesExist -> when doesExist $ removeFile filePath

-- check whether file content between 2 files are same
isSameFile :: MonadIO m => FilePath -> B.ByteString -> m Bool
isSameFile filePath bs = do
  fileContent <- liftIO $ B.readFile filePath
  return $ fileContent == bs
