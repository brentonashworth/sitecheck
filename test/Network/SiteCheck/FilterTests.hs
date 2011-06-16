module Network.SiteCheck.FilterTests where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Data.List (sort)

import Network.SiteCheck.Filter
import Network.SiteCheck.Data
import Network.SiteCheck.URL
import Network.SiteCheck.Arbitrary

tests = [ testCase "links are filtered 1" linksAreFiltered1
        , testCase "links are filtered 2" linksAreFiltered2
        ]

testInput = [ "#a"
            , "test#a"
            , "mailto:b"
            , "top"
            , "/view?name=tim"
            , "javascript:view('c')"
            , "javascript:someFunction('x')"
            , "?name=joe"
            , "list.html?name=joe"
            , "/list?name=joe"
            , "http://a.b.c/limit?f=1&f=2&f=3"
            , "http://e.f.g/m?p=m#top"
            , "http://e.f.g/n?c=m&a=n&b=z"
            , "bottom"
            ]

testMappings = (Mappings [ (notIn ["top", "bottom"])
                         , (transformJs "view" firstJs)
                         , removeJs]
                     
                         [ orderedParams
                         , removeParameters ["p"]]
                     
                         [ (limitCombinations "limit" "f" 1)])

expected1 = [ "http://a.b.c/test"
            , "http://a.b.c/view?name=tim"
            , "http://a.b.c/c"
            , "http://a.b.c/j?name=joe"
            , "http://a.b.c/list.html?name=joe"
            , "http://a.b.c/list?name=joe"
            , "http://a.b.c/limit?f=1"
            , "http://a.b.c/limit?f=2"
            , "http://a.b.c/limit?f=3"
            , "http://e.f.g/m"
            , "http://e.f.g/n?a=n&b=z&c=m"
            ]

linksAreFiltered1 = 
  let Just url = importURL "http://a.b.c/j?x=4"
      results = filterLinks testMappings url testInput
      results' = map exportURL results
  in
  expected1 @=? results'

expected2 = [ "http://a.b.c/home/test"
            , "http://a.b.c/view?name=tim"
            , "http://a.b.c/home/c"
            , "http://a.b.c/home/menu.html?name=joe"
            , "http://a.b.c/home/list.html?name=joe"
            , "http://a.b.c/list?name=joe"
            , "http://a.b.c/limit?f=1"
            , "http://a.b.c/limit?f=2"
            , "http://a.b.c/limit?f=3"
            , "http://e.f.g/m"
            , "http://e.f.g/n?a=n&b=z&c=m"
            ]

linksAreFiltered2 = 
  let Just url = importURL "http://a.b.c/home/menu.html?x=4"
      results = filterLinks testMappings url testInput
      results' = map exportURL results
  in
  expected2 @=? results'
