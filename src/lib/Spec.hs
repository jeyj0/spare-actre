import Test.Hspec
import Test.QuickCheck

import qualified MainCopy as Main

main :: IO ()
main = hspec $ do
  Main.tests
