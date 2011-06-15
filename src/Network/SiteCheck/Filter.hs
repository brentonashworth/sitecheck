
-- | Functions for filtering and transforming URL strings and URLs.

module Network.SiteCheck.Filter
  ( filterLinks
  , notIn
  , transformJs
  , firstJs
  , removeJs
  , orderedParams
  , removeParameters
  , limitCombinations
  , startsWith
  , removeMailTo
  , removeLeadingHash
  , removeHash
  ) where

import Control.Monad (liftM)
import Data.List (isSuffixOf, isPrefixOf, nub)
import Text.Regex (mkRegex, splitRegex)
import Text.Regex.Posix

import Network.SiteCheck.URL
import Network.SiteCheck.Data
import Network.SiteCheck.Util

-- | Using the provided mappings and base URL, transform a list of strings into
-- a list of absolute URLs. Mappings may add, remove or tranform URLs.
filterLinks :: Mappings -> URL -> [String] -> [URL]
filterLinks mappings thisPage xs = 
    nub $
    fromMaybe $
    runToListMaps (urlToListOfURL mappings) $
    runMaps (urlToURL mappings) $
    -- all URLs are absolute
    map (liftM (makeAbsolute thisPage)) $
    -- converted to [Maybe URL]
    map (\x -> importURL =<< x) $
    runMaps (stringToString mappings) $
    runMaps [
                  removeLeadingHash
                , removeHash
                , removeMailTo
                ] $
    -- start with [Maybe String]
    map Just xs

-- | Given a list of mapping functions and list of Maybe values, map the
-- values through each mapping one at a time finally producing a list
-- of Maybe values.
runMaps :: [Mapping a] -> [Maybe a] -> [Maybe a]
runMaps (f:fs) xs =
  runMaps fs $ map (>>= f) xs
runMaps [] xs = xs

-- | Given a list of one-to-many mappings and list of Maybe values, apply
-- the one-to-many mappings to produce a possibly larger list of Maybe values.
runToListMaps :: [(a -> [Maybe a])] -> [Maybe a] -> [Maybe a]
runToListMaps (f:fs) xs = runToListMaps fs $ concat $ map m xs
  where m x = case x of
                Just y -> (f y)
                Nothing -> [Nothing]
runToListMaps [] xs = xs

-- Some internal mapping functions
-- ===============================

-- | Create a mapping function which will filter Strings with a specific 
-- prefix.
startsWith :: String -> Mapping String
startsWith p x = if p `isPrefixOf` x
                 then Nothing
                 else Just x

-- | Mapping for removing mailto strings.
removeMailTo :: Mapping String
removeMailTo = startsWith "mailto:"
  
-- | Mapping for removing strings with a leading hash. Hashes don't matter when
-- checking link status, so we remove them.
removeLeadingHash :: Mapping String
removeLeadingHash = startsWith "#"
  
-- | Mapping for removing a hash from withing a string.
removeHash :: Mapping String
removeHash = safeHead . splitRegex (mkRegex "#")
  
-- Some mappings you may want to use
-- ================================

-- | Create a mapping that will remove any string that does not appear in the
-- provided list.
notIn :: [String] -> Mapping String
notIn list x = if x `elem` list
               then Nothing
               else Just x

-- | A mapping which will order the parameters in alpha order by parameter 
-- name. This is useful when you may encounter many URL with the same
-- parameters but in different order. By ordering them you ensure that the
-- resource is visited only once.
orderedParams :: Mapping URL
orderedParams url = return $ orderParams url

-- | Create a mapping which will remove the parameters with names that match
-- those in the provided list.
removeParameters :: [String] -> Mapping URL
removeParameters params x =
  return $ foldr removeParam x params

-- | Create a mapping for limiting the number of combinations of a single
-- parameter. You provide a URL pattern, the parameter name and the max
-- number of times the parameter may appear in a single URL.
--
-- See documentation for 'fixedCombinations' for more information.
limitCombinations :: String -> String -> Int -> URL -> [Maybe URL]
limitCombinations purl param n url =
  if (exportURL url) =~ purl
  then map Just $ fixedCombinations param n url
  else [Just url]

-- | If an href contains a javascript call which does something with a URL 
-- string you may use this function to create a mapping which extracts this
-- string so that it can become a testable URL. Given a javascript function 
-- name, and a function which converts the javascript function parameters into 
-- a URL string, return a String mapping.
transformJs :: String -> Mapping String -> Mapping String
transformJs name f x = 
  if x =~ ("^javascript:"++name++".*")
  then let (_, y, _) = x =~ "[(]['].*['][)]" :: (String, String, String) in
       f y
  else Just x

-- | This function can be used with tranformJs to produce a mapping which
-- will return the first argument of a javascript function.
firstJs :: Mapping String
firstJs = \y -> 
  safeHead $
  splitRegex (mkRegex "', '") $
  drop 2 $
  take ((length y) - 2) y

-- | A Mapping which will remove any javascript functions.
removeJs :: Mapping String
removeJs x =
  if x =~ ("^javascript:.*")
  then Nothing
  else Just x
