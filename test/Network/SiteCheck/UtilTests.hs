module Network.SiteCheck.UtilTests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, State)

import Network.SiteCheck.Util

tests :: [Test]
tests = [ testCase "safe head non-empty" safeHeadPresent
        , testCase "safe head empty" safeHeadAbsent
        , testProperty "fromMaybe <= input list" prop_FromMaybe
        ]

safeHeadPresent = (Just 'h') @=? safeHead "hello"
safeHeadAbsent = Nothing @=? (safeHead [] :: Maybe String)

prop_FromMaybe xs = (length (fromMaybe xs)) <= (length xs) 
  where types = (xs :: [Maybe Int])
