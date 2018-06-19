import Prelude
import System.Environment
import Test.Hspec

import qualified Platform.SQLite as DB
import qualified Spec.Database as SpecDB
import Uploader.File

main :: IO ()
main = do
  setEnv "DATABASE_URL" dbName
  deleteFile dbName
  hspec $ do
    SpecDB.spec

  where
    dbName = "/tmp/test.db"
