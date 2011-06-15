module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Network.SiteCheckTests
import qualified Network.SiteCheck.UtilTests
import qualified Network.SiteCheck.URLTests
import qualified Network.SiteCheck.DataTests
import qualified Network.SiteCheck.FilterTests

main :: IO ()
main = defaultMain
    [ testGroup "Util Tests" Network.SiteCheck.UtilTests.tests
    , testGroup "URL Tests" Network.SiteCheck.URLTests.tests
    , testGroup "Data Tests" Network.SiteCheck.DataTests.tests
    , testGroup "Script Tests" Network.SiteCheckTests.tests
    , testGroup "Filter Tests" Network.SiteCheck.FilterTests.tests
    ]
