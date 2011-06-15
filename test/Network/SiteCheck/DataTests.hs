module Network.SiteCheck.DataTests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, State)
import Data.List (nub)

import Network.SiteCheck.Data
import Network.SiteCheck.URL

import Network.SiteCheck.Arbitrary

tests :: [Test]
tests = [ testCase "link status is not 200" statusOkFalse
        , testCase "link status is 200" statusOkTrue
        , testCase "header exists" headerPresent
        , testCase "header does not exist" headerAbsent
        , testProperty "distinct links" prop_distinct_links
        ]

mkResponse :: Response
mkResponse = (Response { rStatus = 200
                       , rHeaders = [("a"," b")]
                       , rBody = ""})  

testLink :: String -> [URL] -> Int -> String -> Link
testLink p prev code l =
  let Just p' = importURL p
      Just l' = importURL l
  in
  (Link p' prev (Code code) l')

emptyLink = testLink "" [] 200 ""

statusOkFalse = False @=? statusOk (testLink "" [] 302 "")
statusOkTrue = True @=? statusOk (testLink "" [] 200 "")

headerPresent = (Just "b") @=? header mkResponse "a"
headerAbsent = Nothing @=? (header mkResponse "b" :: Maybe String)

-- | distinctLinks procudes a distinct list of links
prop_distinct_links ls = 
  let dl = distinctLinks ls in
  (dl == nub dl) && ((u dl) == nub (u dl)) 
  where u xs = map (exportURL . toURL) xs
