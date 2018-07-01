{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.HTTP
      ( main
      ) where

import Data.Maybe
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as LT

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse

import System.Environment

import qualified Uploader.HTTP as UploaderHTTP
import Uploader.File

type App r m = (MonadIO m, UploaderHTTP.Service m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  uploadDir <- uploadBasePath
  scottyT (read port :: Int) runner $ routes uploadDir
  where
    acquirePort = fromMaybe "3000" <$> lookupEnv "PORT"

-- * Routing

routes :: (App r m) => String -> ScottyT LT.Text m ()
routes uploadDir = do
  -- middlewares

  middleware logStdoutDev
  middleware $ staticPolicy (noDots <> addBase uploadDir)

  middleware $ cors $ const $ Just simpleCorsResourcePolicy
    { corsRequestHeaders = "Authorization":simpleHeaders
    , corsMethods = "PUT":"DELETE":simpleMethods
    }
  options (regex ".*") $ return ()

  -- err
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  UploaderHTTP.routes

  -- health
  get "/api/health" $
    json True
