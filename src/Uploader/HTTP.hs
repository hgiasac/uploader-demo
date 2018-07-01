{-# LANGUAGE OverloadedStrings #-}
module Uploader.HTTP where

import Web.Scotty.Trans
import Network.Wai.Parse
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Data.Text.Encoding
import Control.Monad.Trans.Class

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import Uploader.Types
import qualified Uploader.File as UF

headerFileName :: LT.Text
headerFileName = "X-File-Name"

headerContentType :: LT.Text
headerContentType = "Content-Type"

class (Monad m) => Service m where
  createFile :: String -> FileInfo B.ByteString -> m (Either UploadFileError CreateUploadFile)
  getFile :: String -> m (Either UploadFileError UploadFile)
  deleteFile :: String -> m (Either UploadFileError ())


routes :: (MonadIO m, Service m) => ScottyT LT.Text m ()
routes = do

  post "/uploads" $ do
    fs <- files
    uploadDir <- liftIO $ UF.uploadBasePath
    results <- mapM
      (stopIfError uploadFileErrorHandler . createFile uploadDir . snd) fs
    json results

  post "/upload-direct" $ do
    uploadDir <- liftIO $ UF.uploadBasePath
    fName <- LT.unpack <$> validateHeader headerFileName
    contentType <- LT.unpack <$> validateHeader headerContentType
    fc <- body
    result <- lift $ createFile uploadDir $ FileInfo (BS.pack fName)
      (BS.pack contentType) fc
    case result of
      Right model -> json [model]
      Left err -> uploadFileErrorHandler err


  get "/uploads/:name" $ do
    name <- param "name"
    result <- lift $ getFile name
    case result of
      Right model -> do
        addHeader "Content-Type" $ LT.pack (uploadFileType model)
        file $ uploadFileURI model
      Left err -> uploadFileErrorHandler err

  delete "/uploads/:name" $ do
    name <- param "name"
    result <- lift $ deleteFile name
    case result of
      Right _ -> status status204
      Left err -> uploadFileErrorHandler err

-- Error Handler
uploadFileErrorHandler ::  (ScottyError e, Monad m)
  => UploadFileError -> ActionT e m ()
uploadFileErrorHandler err = case err of
  UploadFileBadRequest e -> do
    status status400
    json $ ErrorWrapper e
  UploadFileNotFound _ -> do
    status status404
    json $ ErrorWrapper "File Not found"

-- Stop request if error
stopIfError :: (Monad m, ScottyError e') => (e -> ActionT e' m ()) -> m (Either e a) -> ActionT e' m a
stopIfError errHandler action = do
  result <- lift action
  case result of
    Left e -> do
      errHandler e
      finish
    Right a ->
      return a


-- validate Empty
validateHeader :: (Monad m, ScottyError e') => LT.Text -> ActionT e' m LT.Text
validateHeader fieldName = do
  mVal <- header fieldName
  stopIfError uploadFileErrorHandler
    (return $ checkEmpty fieldName mVal)

checkEmpty :: LT.Text -> Maybe LT.Text -> Either UploadFileError LT.Text
checkEmpty fieldName Nothing
  = Left $ UploadFileBadRequest $ LT.pack (LT.unpack fieldName ++ " is required")
checkEmpty fieldName (Just "")
  = Left $ UploadFileBadRequest $ LT.pack (LT.unpack fieldName ++ " is required")
checkEmpty fieldName (Just val) = Right val
