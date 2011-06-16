{-# LANGUAGE TypeSynonymInstances #-}

-- | All of the datatypes and class definitions which are required to
-- control the process, represent the state, and report on the status, of 
-- crawling a domain. The common feature of everything included in this module
-- is that it supports this core functionality.

module Network.SiteCheck.Data 
    ( URLish(..)
      -- * Links
    , Link(..), StatusCode(..)
    , isRedirect, wasRedirected, isStatusOk, newLinkWithPrev, distinctLinks
      -- * Scripts
    , Script(..), Decision(..), Option(..)
    , getResultFile, isOptionSet, getLimit
      -- * Configuration
    , Curls(..), Config
    , filterResults, isOverLimit
      -- * Visited Links
    , VisitStore(..), Visited
    , emptyVisited
      -- * Crawl State
    , State, Mapping, Mappings(..), Action(..)
    , initState, isNextVisited
    -- * Stack functions
    , getStack, stackAsStrings, stackAsURLs
    , stackLength, isStackEmpty
    , initStack, pushStack, popStack, deleteStack, tailStack, mergeWithStack
      -- * Response
    , Response(..), header
    ) where

import Network.Curl
import Data.List (nubBy)
import qualified Data.Map as M

import Network.SiteCheck.URL
import Network.SiteCheck.Util

-- | Some functions only need to know that a datatype is URLish.
class URLish a where
  toURL :: a -> URL
  toLink :: a -> Link

instance URLish URL where
  toURL u = u
  toLink u = urlToLink u

instance URLish String where
  toURL s = case importURL s of
             Just x  -> x
             Nothing -> emptyURL
  toLink s = toLink $ toURL s

-- | The HTTP status code for a 'Link'.
data StatusCode =   NoCode 
                  | Code Int 
                  deriving (Show, Eq)

-- | A Link holds the URL being tracked, the parent URL where this URL 
-- was found, the final StatusCode and a list of the previous URLs which 
-- redirected to this page.
--
-- When a URL returns a 301 or 302 status code and a location header, a new
-- Link is created with the location as the URL and the old URL added to 
-- the previous list.
data Link = Link {
    parent :: URL        -- ^ the URL where this Link was found
  , previous :: [URL]    -- ^ a list of URLs which redirected to this one
  , status :: StatusCode -- ^ the final status code for this URL
  , theURL   :: URL      -- ^ the URL being tracked
  } deriving (Show, Eq)

-- | A Link is 'URLish'.
instance URLish Link where
  toURL l = (theURL l)
  toLink l = l

-- | Create a Link from a URL.
urlToLink :: URL -> Link
urlToLink url = (Link emptyURL [] NoCode url)

-- | Create a new Link from something URLish adding the 'URL' from the old 
-- Link to the list of previous URLs.
newLinkWithPrev :: URLish a => Link -> a -> Link
newLinkWithPrev old new = 
  (old { previous = (toURL old) : (previous old), theURL = (toURL new)})

-- | Is the 'StatusCode' in a Link 301 or 302?
isRedirect :: Link -> Bool
isRedirect (Link _ _ (Code 301) _) = True
isRedirect (Link _ _ (Code 302) _) = True
isRedirect (Link _ _ _ _) = False

-- | Was this Link the result of a redirect?
wasRedirected :: Link -> Bool
wasRedirected (Link _ (x:xs) _ _) = True
wasRedirected (Link _ _ _ _) = False

-- | Is the 'StatusCode' for this Link 200?
isStatusOk :: Link -> Bool
isStatusOk (Link _ _ (Code 200) _) = True
isStatusOk (Link _ _ _ _) = False

-- | Returns a list of distinct Links. Two Links are considered the same if
-- they have the same textual representation.
distinctLinks :: [Link] -> [Link]
distinctLinks = 
  nubBy (\a b -> (u a) == (u b))
  where u = (exportURL . toURL)

-- | As SiteCheck runs it can be configured to print status information using
-- the following options. When a crawl is complete any non-200 status codes
-- will be reported. If you set AllResults then all results will be shown, if
-- you set RedirectResults then all redirects are shown in addition to
-- failures.
data Option =   Limit Int          -- ^ the maximum number of URLs to crawl
              | ResultFile String  -- ^ a file name where results are printed
              | PrintStatus        -- ^ print each URL as it is retrieved
              | PrintStack         -- ^ print the complete 'Stack'
              | PrintTopStack      -- ^ print the top five Stack entries
              | PrintActions       -- ^ print the actions chosen for each URL
              | PrintPosts         -- ^ print each post action
              | PrintParent        -- ^ print the parent of each URL
              | AllResults         -- ^ show all results when finished
              | RedirectResults    -- ^ show errors and redirects
              deriving (Show, Eq)

-- | Get the 'Limit' value from a list of Options.
getLimit :: [Option] -> Maybe Int
getLimit (x:xs) = 
  case x of
    (Limit l) -> Just l
    _         -> getLimit xs
getLimit []   = Nothing

-- | Get the 'ResultFile' value from a list of Options.
getResultFile :: [Option] -> Maybe String
getResultFile (x:xs) =
  case x of
    ResultFile name -> Just name
    _               -> getResultFile xs
getResultFile [] = Nothing

type Mapping a = (a -> Maybe a)

-- | Mappings control how a string, which comes from an anchor href attribute
-- becomes a 'URL' which will be reported. Each mapping function returns a
-- Maybe value. Because a mapping may return Nothing, it can be used for
-- filtering as well as transforming URLs. 
-- 
-- During the link filtering process raw strings are first passed through 
-- the stringToString mappings then the urlToUrl mappings and finally the
-- urlToListOfURL mappings.
data Mappings = Mappings {
    stringToString :: [Mapping String]
  , urlToURL :: [Mapping URL]
  , urlToListOfURL :: [(URL -> [Maybe URL])]
}

data Script = Script { dn :: String 
                     , options :: [Option]
                     , intOpts :: [CurlOption] 
                     , extOpts :: [CurlOption]
                     , seed :: [String]
                     , decisions :: [Decision]
                     , mappings :: Mappings
                     }

data Decision = Decision { isMatch :: String -> Bool
                         , actions :: [Action]
                         }

data Action =   Ignore
              | PushLinks
              | Post String [(String, String)]
              | Get String [(String, String)]
              deriving (Show)

data Curls = Curls { intra :: Curl, extra :: Curl }

type Config = (Curls, Script)

isOptionSet :: Option -> Config -> Bool
isOptionSet opt (_, script) =
  opt `elem` (options script)

initStack :: Config -> Stack
initStack (_, script) = map urlToLink . 
                        fromMaybe . 
                        map importURL . 
                        seed $ script

isOverLimit :: Config -> State -> Bool
isOverLimit (_, script) state = 
  case getLimit $ options script of
    Just x  -> x <= visitedLength state
    Nothing -> False

filterResults :: Config -> [Link] -> [Link]
filterResults config@(_, script) links
  | (isOptionSet AllResults config) = links
  | (isOptionSet RedirectResults config) = filter redirectedOrNon200 links
  | otherwise = filter (not . isStatusOk) links
  where redirectedOrNon200 x = wasRedirected x || (not . isStatusOk) x

class VisitStore a where
  addVisited :: (URLish b) => b -> StatusCode -> a -> a
  deleteVisited :: (URLish b) => b -> a -> a
  visitedLength :: a -> Int
  exportVisited :: a -> [Link]

newtype Visited = V (M.Map String Link) deriving (Show, Eq)

instance VisitStore Visited where
  addVisited u code (V visited) = 
    let k = (exportURL (toURL u))
        v = ((toLink u) {status = code})
    in
    (V (M.insert k v visited))

  deleteVisited u (V visited)  = 
    let k = (exportURL (toURL u)) in
    (V (M.delete k visited))

  visitedLength (V v) = M.size v

  exportVisited (V v) = M.elems v

emptyVisited :: Visited
emptyVisited = V M.empty

isVisited :: (URLish a) => Visited -> a -> Bool
isVisited (V visited) u = M.member (exportURL (toURL u)) visited

notVisited :: (URLish a) => Visited -> a -> Bool
notVisited visited u = (not (isVisited visited u))

distinct :: Visited -> [Link] -> [URL] -> [URL]
distinct visited queue new = 
  filter (`notElem` queueStrings) $
  filter (notVisited visited)  new
  where queueStrings = map theURL queue

type Stack = [Link]

data State = State Visited Stack 
             deriving (Show, Eq)

instance VisitStore State where
  addVisited l code (State visited stack) = 
    (State (addVisited l code visited) stack)

  deleteVisited l (State v s) = (State (deleteVisited l v) s)

  visitedLength (State v _) = visitedLength v

  exportVisited (State v _) = exportVisited v

initState :: Visited -> Stack -> State
initState v s = State v s

isNextVisited :: State -> Bool
isNextVisited (State v (x:xs)) = isVisited v x
isNextVisited (State _ []) = False

tailStack :: State -> State
tailStack (State v (x:xs)) = State v xs
tailStack state@(State _ []) = state

deleteStack :: State -> State
deleteStack (State v _) = State v []

isStackEmpty :: State -> Bool
isStackEmpty (State _ (x:xs)) = False
isStackEmpty _                = True

popStack :: State -> (Maybe Link, State)
popStack (State v (x:xs)) = (Just x, (State v xs))
popStack s@(State v []) = (Nothing, s)

pushStack :: (URLish a) => a -> State -> State
pushStack l (State v xs) = (State v ((toLink l) : xs))

getStack :: State -> [Link]
getStack (State _ xs) = xs

stackLength :: State -> Int
stackLength (State _ stack) = length stack

stackAsURLs :: State -> [URL]
stackAsURLs = map toURL . getStack

stackAsStrings :: State -> [String]
stackAsStrings = map exportURL . stackAsURLs

mergeWithStack :: Link -> [URL] -> State -> State
mergeWithStack parent new (State visited stack) = 
  (State visited) $
  (stack ++) $ 
  (map (Link (theURL parent) [] NoCode)) $
  distinct visited stack new

data Response =   Error 
                | Response { rStatus :: Int
                           , rHeaders :: [(String, String)]
                           , rBody :: String} 
                deriving (Show, Eq)

-- | Lookup a header and remove any leading spaces.
header :: Response -> String -> Maybe String
header r k = case r of
               Error -> Nothing
               (Response _ hs _) -> do
                 h <- lookup k hs
                 return (dropWhile (== ' ') h)
