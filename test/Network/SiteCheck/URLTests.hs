{-# LANGUAGE TypeSynonymInstances #-}

module Network.SiteCheck.URLTests where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Data.List (sort)

import Network.SiteCheck.URL
import Network.SiteCheck.Arbitrary

tests = [ testCase "remove params with params" removeParamsWithParams
        , testCase "remove params without params" removeParamsWithoutParams
        , testCase "remove params only params" removeParamsOnlyParams
        , testProperty "params are removed" prop_params_are_removed
        , testProperty "param is added" prop_param_is_added     
        , testProperty "param is removed" prop_param_is_removed     
        , testProperty "params are ordered" prop_params_are_ordered     
        , testProperty "host is added" prop_host_is_added
        , testCase "fixed combinations 1" fixedCombinations1
        , testCase "fixed combinations 2" fixedCombinations2
        , testCase "is in domain" itIsInDomain
        , testCase "is not in domain" itIsNotInDomain
        , testCase "is in domain with port" itIsInDomainWithPort
        , testCase "is not in domain with port" itIsNotInDomainWithPort
        , testCase "merge paths 1" mergePaths1
        , testCase "merge paths 2" mergePaths2
        , testCase "merge paths 3" mergePaths3
        , testCase "merge paths 4" mergePaths4
        , testCase "merge paths 5" mergePaths5
        , testCase "merge paths 6" mergePaths6
        , testCase "merge paths 7" mergePaths7
        ]

roundtrip :: (URL -> URL) -> String -> String
roundtrip f x = case importURL x of
                  Just u  -> exportURL . f $ u
                  Nothing -> x

removeParamsWithParams = 
  "http://a.b.c/d" @=? roundtrip removeParams "http://a.b.c/d?e=f"
removeParamsWithoutParams =
  "http://a.b.c/d" @=? roundtrip removeParams "http://a.b.c/d"
removeParamsOnlyParams = "" @=? roundtrip removeParams "?e=f"

prop_params_are_removed url = (url_params (removeParams url)) == []
 where types = (url :: URL)

prop_param_is_added url = 
  let url' = (addParam ("name", "joe") url) in
    (lookup "name" (url_params url')) == (Just "joe")
  where types = (url :: URL)

prop_param_is_removed url
  | (url_params url) == [] =
    (url_params (removeParam "name" url)) == [] -- no params
  | otherwise =
    let params = (url_params url)
        count = (length params)
        (n, v) = head params
        url' = (removeParam n url)
    in (length $ url_params url') == count - 1 &&
       lookup n (url_params url') == Nothing

prop_params_are_ordered url = 
  (url_params (orderParams url)) == sort (url_params url)

prop_host_is_added old =
  let Just base = importURL "http://www.company.com/some/page?name=dan&age=20"
      new = (makeAbsolute base old)
  in case (url_type old) of
       PathRelative -> (url_type new)   == (url_type base) && 
                       (url_path new)   == (url_path base) >/< (url_path old) &&
                       (url_params new) == (url_params old)
       HostRelative -> (url_type new)   == (url_type base) &&
                       (url_path new)   == (url_path old) &&
                       (url_params new) == (url_params old)
       Absolute _   -> new == old

fixedCombinations1 = 
  let Just url = importURL "http://c.c/page?a=1&a=2" in
 [ [("a","1")]
 , [("a", "2")]
 ] @=? (map url_params $ fixedCombinations "a" 1 url)

fixedCombinations2 = 
  let Just url = importURL "http://c.c/page?a=1&a=2&a=3" in
 [ [("a","1"), ("a", "2")]
 , [("a","1"), ("a", "3")]
 , [("a", "2"), ("a", "1")]
 , [("a", "2"), ("a", "3")]
 , [("a", "3"), ("a", "1")]
 , [("a", "3"), ("a", "2")]
 ] @=? (map url_params $ fixedCombinations "a" 2 url)

itIsInDomain =
  let Just url = importURL "http://a.b.c/test" in
  True @=? isInDomain "a.b.c" url

itIsNotInDomain =
  let Just url = importURL "http://a.b.c/test" in
  False @=? isInDomain "a.b.d" url

itIsInDomainWithPort =
  let Just url = importURL "http://a.b.c:10/test" in
  True @=? isInDomain "a.b.c:10" url

itIsNotInDomainWithPort =
  let Just url = importURL "http://a.b.c:30/test" in
  False @=? isInDomain "a.b.c:10" url

mergePaths1 = ""          @=? ""      >/< ""
mergePaths2 = ""          @=? "a"     >/< ""
mergePaths3 = "b"         @=? ""      >/< "b"
mergePaths4 = "b"         @=? "a"     >/< "b"
mergePaths5 = "a/c"       @=? "a/b"   >/< "c"
mergePaths6 = "a/c/d"     @=? "a/b"   >/< "c/d"
mergePaths7 = "a/b/d/e/f" @=? "a/b/c" >/< "d/e/f"
  
