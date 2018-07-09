module Uploader.Util where

import Data.List
import Data.List.Split
import Network.Wai.Parse
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import System.FilePath ((</>))
import Uploader.Types

-- append file name with surfix
appendFileNameSurfix :: String -> String -> String
appendFileNameSurfix name surfix
  | length parts <= 1 = name ++ "_" ++ surfix
  | otherwise = intercalate "_" (init parts ++ [surfix]) ++ "." ++ last parts
    where
      parts = splitOn "." name


-- exrtact create upload file data from file info
extractFileInfo :: FilePath ->  FileInfo B.ByteString -> Int -> CreateUploadFile
extractFileInfo filePath f isLink = CreateUploadFile
  { createUploadFileName = fName
  , createUploadFileSize = B.length $ fileContent f
  , createUploadFileType = BS.unpack $ fileContentType f
  , createUploadFileIsLink = isLink
  , createUploadFileURI = filePath </> fName }
  where
    fName = BS.unpack $ fileName f


-- find files with same content types and size
filterSameFiles :: CreateUploadFile -> [UploadFile] -> [UploadFile]
filterSameFiles cf = filter
  (\f -> uploadFileType f == createUploadFileType cf
         && uploadFileSize f == createUploadFileSize cf)

-- Calculate unique file name from outside existed name list
calculateUniqueFileName :: String -> [String] -> String
calculateUniqueFileName name [] = name
calculateUniqueFileName name ns = findUniqueFileName' 0 name ns
  where
    findUniqueFileName' x n ns'
      = if uName `notElem` ns'
        then uName
        else findUniqueFileName' (x + 1) n ns'
      where
        uName = if x == 0
          then n
          else appendFileNameSurfix n (show x)
