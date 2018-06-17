{-# LANGUAGE OverloadedStrings #-}
module Uploader.HTTP where

import Web.Scotty.Trans
import Network.Wai.Parse
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import Uploader.File
import qualified Data.ByteString.Char8 as BS

routes :: MonadIO m => ScottyT LT.Text m ()
routes = do

  post "/upload" $ do
    fs <- files
    liftIO $ mapM (saveFile "uploads" . snd) fs
    html "Successfully"

  post "/upload-direct" $ do
    fName <- fmap (maybe "test" LT.unpack) (header "X-File-Name")
    contentType <- fmap (maybe "jpg" LT.unpack) (header "content-type")
    fc <- body
    saveFile "uploads" $ FileInfo (BS.pack fName) (BS.pack contentType) fc
    html "Successfully"
