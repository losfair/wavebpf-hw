import Prelude

import Test.Tasty

import qualified Tests.Wbcore.Project

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Wbcore.Project.tests
  ]
