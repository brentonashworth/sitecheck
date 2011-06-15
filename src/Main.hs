
-- | A simple example. Submit a search and check 20 pages at www.google.com. 
-- This example shows a decision which results in a Get request. To Post, 
-- you would only need to change Get to Post.

module Main where

import Network.Curl
import Text.Regex.Posix
import Network.SiteCheck
import Network.SiteCheck.URL

domain = "www.google.com"
httpG = "http://" ++ domain
userAgent = "SiteCheck 0.0.4"

-- | If we encounter the main google page then submit a search, otherwise
-- add links to the stack and continue.
sampleDecisions = 
  [ Decision 
    { isMatch = (=~ httpG)
    , actions = [ PushLinks
                , (Get httpG [("q", "haskell sitecheck")])
                ]
    }                                        
  , Decision {isMatch = (\x -> True), actions = [PushLinks]}
  ]

-- | Mappings operate on the list of links that are pulled from a page. Anything
-- in an a href is retrieved.
sampleMappings =
  Mappings { stringToString = [] -- transform strings before they become urls
           , urlToURL = [noWebhp] -- tranform urls
           , urlToListOfURL = [] -- turn one url into many
           }

-- Even though mapper function are used to map over a list of Strings or URLs,
-- they may also be used to filter data. Each mapper function returns its 
-- result wrapped in a Maybe. Nothing values will be filtered out.

-- | Don't look at urls where the path is "webhp"
noWebhp :: URL -> Maybe URL
noWebhp url = if (url_path url) == "webhp"
              then Nothing
              else Just url

-- Create a script. You may use any of Network.Curls options in intOpts and
-- extOpts. intOpts are the options for the curl handle that is used for
-- intra-domain requests and the extOpts are the options for the curl handle
-- which is used for requests outside the domain. It is useful to have 
-- different options for these two categories of requests.
--
-- seed is the list of initial URLs to get the process started.
--
-- All non-200 results are always output. The AllResults and RedirectResults
-- options provide additional output.

sampleScript = Script { 
    dn = domain
  , options = [Limit 20
--              , AllResults       -- Show all results, even 200
--              , RedirectResults  -- Show non 200 and any redirect pages
              , PrintStatus        -- Print each URL as it is being checked
--              , PrintStack       -- Print everything in the stack
--              , PrintTopStack    -- Print the top 5 base pages in the stack
--              , PrintActions     -- Print the actions for each page
--              , PrintParent      -- Print the parent for each page
--              , PrintPosts       -- Print Post information
              , ResultFile "results.txt" -- Write results to a file
              ]
  , intOpts = [ CurlCookieJar "intraCookies"
--              , CurlVerbose True
              , CurlUserAgent userAgent 
              ]
  , extOpts = [ CurlCookieJar "extraCookies"
--              , CurlVerbose True
              , CurlUserAgent userAgent
              ]
  , seed = [httpG]
  , decisions = sampleDecisions
  , mappings = sampleMappings
  }

-- Run the script

main = executeScript sampleScript
