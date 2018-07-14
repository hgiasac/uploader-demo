{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Platform.SQLite where

import Data.Maybe
import Data.Pool
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Data.Has
import Database.SQLite.Simple
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>))
import System.Environment

type Env = Pool Connection

type DB r m = (MonadReader r m, Has Env r, MonadIO m)

init :: IO Env
init = do
  pool <- acquirePool
  migrateDb pool
  return pool

acquirePool :: IO Env
acquirePool = do
  uri <- (\x -> if x == "" then "files.db"
                else x) . fromMaybe "" <$> lookupEnv "DATABASE_URL"
  createPool (open uri) close 1 10 10


migrateDb :: Env -> IO ()
migrateDb pool = withResource pool $ \conn -> do
  qry <- Query <$> TextIO.readFile ("migration" </> "db.sql")
  execute_ conn qry


withConn :: DB r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool action
