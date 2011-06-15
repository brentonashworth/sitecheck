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

prop_host_is_added url =
  let Just root = importURL "http://www.company.com/some/page?name=dan&age=20"
      url' = (makeAbsolute root url)
  in case (url_type url) of
       PathRelative -> (url_type url')   == (url_type root) && 
                       (url_path url')   == (url_path url) &&
                       (url_params url') == (url_params url)
       HostRelative -> (url_type url')   == (url_type root) &&
                       (url_path url')   == (url_path url) &&
                       (url_params url') == (url_params url)
       Absolute _   -> url' == url

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
