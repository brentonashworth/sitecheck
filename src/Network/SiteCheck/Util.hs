
-- | Utility functions which are likely to be replaced by core Haskell 
-- functions once I become better aquainted with the libraries.

module Network.SiteCheck.Util where

import Text.Regex (mkRegex, splitRegex)

-- | Given a list of Maybe values, remove the Nothing values and return a list
-- of unpacked values.
fromMaybe :: [Maybe a] -> [a]
fromMaybe xs = foldr g [] xs
               where g next acc = 
                       case next of
                         Just x  -> (x:acc)
                         Nothing -> acc

-- | Safely get the head.
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

splitWith :: String -> String -> [String] 
splitWith s = splitRegex (mkRegex s)
