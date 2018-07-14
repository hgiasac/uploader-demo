module Uploader.Util where

import Data.Ratio
import Data.List
import Data.List.Split
import Network.Wai.Parse
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock.POSIX
import System.FilePath ((</>))
import Uploader.Types
import System.FilePath.Posix (splitExtension)

-- append file name with surfix
appendFileNameSurfix :: String -> String -> String
appendFileNameSurfix name surfix
  | length parts <= 1 = name ++ "_" ++ surfix
  | otherwise = intercalate "_" (init parts ++ [surfix]) ++ "." ++ last parts
    where
      parts = splitOn "." name


-- exrtact create upload file data from file info
extractFileInfo :: FilePath ->  FileInfo B.ByteString -> Int -> UploadFile
extractFileInfo filePath f isLink =
  UploadFile
    { uploadFileName = fName
    , uploadFileSize = B.length $ fileContent f
    , uploadFileType = BS.unpack $ fileContentType f
    , uploadFileIsLink = isLink
    , uploadFileURI = filePath </> fName }
  where
    fName = BS.unpack $ fileName f


-- find files with same content types and size
filterSameFiles :: UploadFile -> [UploadFile] -> [UploadFile]
filterSameFiles cf = filter
  (\f -> uploadFileType f == uploadFileType cf
         && uploadFileSize f == uploadFileSize cf)

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


timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

randomFileName :: String -> IO String
randomFileName fName = (\x -> name ++ "_" ++ show x ++ ext) <$> timeInMillis
  where
   (name, ext) = splitExtension fName
