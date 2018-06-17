import Prelude
import System.Environment
import Test.Hspec

import qualified Platform.SQLite as DB
import qualified Spec.Database as SpecDB

main :: IO ()
main = do
  setEnv "DATABASE_URL" "test.db"
  hspec $ do
    SpecDB.spec
