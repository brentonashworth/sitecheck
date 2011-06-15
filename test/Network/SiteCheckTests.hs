module Network.SiteCheckTests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, State)
import Data.List (nub)

import Network.SiteCheck
import Network.SiteCheck.URL

import Network.SiteCheck.Arbitrary

tests :: [Test]
tests = [ testCase "non200Resp is passed 200" non200Resp200
        , testCase "non200Resp is passed 302" non200Resp302
        , testProperty "versions are not changed" prop_visited_not_changed
        , testProperty "all stack urls are absolute" prop_all_stack_absolute
        , testProperty "link not in previous" prop_link_not_in_previous
        , testProperty "push links no stack dups" prop_pushlinks_no_stack_dups
        , testProperty "push links no dups" prop_pushlinks_no_dups
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

response :: Int -> Response
response code = (Response { rStatus = code
                          , rHeaders = [("Location", "/redirect")]
                          , rBody = ""})
startState = newState emptyVisited [(toLink "http://a.b.c/test")]

non200Resp200 = startState @=? non200Resp emptyLink mkResponse startState
non200Resp302 = 
  let l = toLink "http://www.test.com/some/page"
      r = response 301
      expLink = toLink "http://www.test.com/redirect"
      expLink' = expLink {previous = [(theURL l)]}
      expected = pushStack expLink' startState
  in
  expected @=? non200Resp l r startState

-- | A call to non200Resp should never change the visited map
prop_visited_not_changed l resp state =
  let before = exportVisited state
      after = exportVisited $ non200Resp l resp state
  in before == after

-- | All URLs in the stack should be Absolute URLs
prop_all_stack_absolute l resp state =
  let s = non200Resp l resp state in
  and $ map eqPart $ stackURLs s
  where eqPart url = case (url_type url) of
                       Absolute _ -> True
                       _          -> False

-- | No Link should have a link value that also in the previous list
prop_link_not_in_previous l resp state =
  let s = non200Resp l resp state in
  and $ map prevNotElem $ stack s
  where prevNotElem x = (toURL x) `notElem` (previous x)

-- | There must be no duplicates in the stack.
prop_pushlinks_no_stack_dups script link resp state =
  let s = pushLinks200 script link resp state 
      after = stackURLs s
  in
  after == nub after

-- | Queuing links should produce no duplicates in the push.
prop_pushlinks_no_dups script l resp state =
  let s = pushLinks200 script l resp state
      all = map (exportURL . theURL) $ (exportVisited s) ++ (stack s)
  in
  all == nub all
