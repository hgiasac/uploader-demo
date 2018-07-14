{-# LANGUAGE OverloadedStrings #-}
module Spec.Database where

import Prelude
import Test.Hspec
import Control.Monad.Reader
import Uploader.Database
import Uploader.Types
import qualified Platform.SQLite as DB

spec :: Spec
spec = fileDbSpec

createFileInput :: UploadFile
createFileInput =  UploadFile
            { uploadFileName = "test.png"
            , uploadFileType = "png"
            , uploadFileSize = 2000
            , uploadFileURI = "uploads/test.png"
            , uploadFileIsLink = 0 }


fileDbSpec :: Spec
fileDbSpec =
  describe "file crud" $ do

    it "should insert successfully" $ do

      dbEnv <- DB.init
      runReaderT (insertFile createFileInput) dbEnv
      -- 1 `shouldBe` 1

    it "should find by name successfully" $ do

      dbEnv <- DB.init
      result <- runReaderT (findFile "test.png") dbEnv
      (uploadFileName <$> result) `shouldBe` Just "test.png"

    it "should delete by name successfully" $ do

      dbEnv <- DB.init
      runReaderT (deleteFile "test.png") dbEnv
      -- 1 `shouldBe` 1
