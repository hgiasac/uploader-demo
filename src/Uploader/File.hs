module Uploader.File where

import Network.Wai.Parse
import Control.Monad.IO.Class
import Data.List.Split
import Data.List

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import System.FilePath
import System.Directory (doesFileExist)

appendFileNameSurfix :: String -> String -> String
appendFileNameSurfix name surfix
  | length parts <= 1 = name ++ "_" ++ surfix
  | otherwise = intercalate "_" (init parts ++ [surfix]) ++ "." ++ last parts
    where
      parts = splitOn "." name

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
saveFile :: MonadIO m => FilePath -> FileInfo B.ByteString -> m FilePath
saveFile uploadPath file = do
  fName <- liftIO $ correctFileName uploadPath $ BS.unpack (fileName file)
  liftIO $ B.writeFile (uploadPath </> fName) fc
  return fName
  where
    fc = fileContent file
