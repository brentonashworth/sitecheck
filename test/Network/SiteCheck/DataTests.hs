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
        , testProperty "identify redirects" prop_identify_redirects
        , testProperty "identify was redirected" prop_was_redirected
        , testCase "get correct limit" getCorrectLimit
        , testCase "no limit" noLimit
        , testCase "get correct result file" getCorrectResultFile
        , testCase "no result file" noResultFile
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

statusOkFalse = False @=? isStatusOk (testLink "" [] 302 "")
statusOkTrue = True @=? isStatusOk (testLink "" [] 200 "")

headerPresent = (Just "b") @=? header mkResponse "a"
headerAbsent = Nothing @=? (header mkResponse "b" :: Maybe String)

-- | distinctLinks produces a distinct list of Links
prop_distinct_links ls = 
  let dl = distinctLinks ls in
  (dl == nub dl) && ((u dl) == nub (u dl)) 
  where u xs = map (exportURL . toURL) xs

prop_identify_redirects link =
  let (Link _ _ code _) = link in
  case code of
    NoCode   -> not . isRedirect $ link
    (Code x) -> if x == 301 || x == 302
                then isRedirect link
                else not . isRedirect $ link

prop_was_redirected link = 
  let (Link _ prev _ _) = link in
  case prev of
    (x:xs) -> wasRedirected link
    []     -> not . wasRedirected $ link

getCorrectLimit =
  Just 10 @=? getLimit [PrintStatus, Limit 10, PrintPosts]
noLimit =
  Nothing @=? getLimit [PrintStatus, PrintPosts]

getCorrectResultFile =
  Just "file.txt" @=? getResultFile [ResultFile "file.txt", Limit 10]
noResultFile =
  Nothing @=? getResultFile [PrintStatus, PrintPosts]
