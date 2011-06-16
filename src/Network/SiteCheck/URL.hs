
-- | A layer on top of "Network.URL" which adds functions for manipulating
-- parameters. 

module Network.SiteCheck.URL 
  ( module Network.URL
  , modifyParams
  , removeParam
  , removeParams
  , addParam
  , orderParams
  , makeAbsolute
  , fixedCombinations
  , emptyURL
  , isInDomain
  , (>/<)
  ) where

import Network.URL
import Data.List (partition, sort, intersperse)

import Network.SiteCheck.Util

type Param = (String, String)

emptyURL = URL PathRelative "" []

-- | Given a function which modifies parameters, apply this to a URL
-- creating a new URL with the modified parameters.
modifyParams :: ([Param] -> [Param]) -> URL -> URL
modifyParams f url = 
  URL (url_type url) (url_path url) . f . url_params $ url

-- | Remove a parameter from a URL.
removeParam :: String -> URL -> URL
removeParam param url = modifyParams (filter (\x -> param /= fst x)) url
                        
-- | Add a new parameter to a URL.
addParam :: Param -> URL -> URL
addParam = flip add_param

-- | Order parameters in alpha order by parameter name.
orderParams :: URL -> URL
orderParams = modifyParams sort

-- | Remove all parameters from a URL.
removeParams :: URL -> URL
removeParams (URL t p _) = URL t p []

-- | Create a new path from two paths.
(</>) :: String -> String -> String
"" </>  b = b
a  </> "" = a
a  </>  b = a ++ "/" ++ b

-- | Merge two paths where the second path is path-relative to the first.
(>/<) :: String -> String -> String
a  >/<  b = (relative a) </> b
            where relative x = concat $ 
                               intersperse "/" $
                               (\xs -> take (length xs - 1) xs) $ 
                               splitWith "/" x

-- | Use the information in the first URL parameter to make the second URL
-- absolute. This will only return a new URL is the type of the second
-- URL is HostRelative or PathRelative.
makeAbsolute :: URL -> URL -> URL
makeAbsolute (URL t path params) url = 
  case url_type url of
    PathRelative -> URL t (prefixPath path url) (url_params url)
    HostRelative -> URL t (url_path url) (url_params url)
    Absolute _   -> url
  where prefixPath path url = path >/< (url_path url)
   
-- | Sometimes a URL may encode a list of options such as:
-- 
-- >   colors=red&colors=blue&colors=green 
--
-- When crawling a site, links may be discovered which have every possible 
-- combination of these values. For a small number of values this is not a 
-- problem. For large numbers x, x! becomes very large and we don't need to 
-- check every possible combination.
--
-- This function will take a URL with n options and produce multiple URLs, each
-- with fewer options. For example, if there are 10 different colors that 
-- may be present then a call to 
--
-- >   fixedCombinations "color" 2 <url> 
--
-- will reduce the search space from 3628800 to 90.
fixedCombinations :: String -> Int -> URL -> [URL]
fixedCombinations s n url =
  let params = url_params url
      (t, o) = partition ((s ==) . fst) params
      values = map snd t
  in map mkURL $ combine $ replicate n values
  where mkURL xs = foldr addP (URL (url_type url) (url_path url) []) xs
        addP next acc = addParam (s, next) acc

-- | Given any number of sets, take the Cartesian product of the sets and
-- remove duplicates.
combine :: (Eq a) => [[a]] -> [[a]]
combine [] = [[]]
combine (set:sets) = let cp = combine sets in
                     [x:xs | x <- set, xs <- cp, x `notElem` xs]

-- | Return True if the URL is in the provided domain.
isInDomain :: String -> URL -> Bool
isInDomain d url = case url_type url of
                     Absolute (Host _ host _) -> host == d
                     _                        -> False
                   
