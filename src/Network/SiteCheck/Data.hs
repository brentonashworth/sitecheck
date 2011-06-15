{-# LANGUAGE TypeSynonymInstances #-}

-- | Datatypes and class definitions for SiteCheck. Definitions and associated
-- functions for Links, Scripts, Configuration, State and Responses.

module Network.SiteCheck.Data where

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

-- Links hold all of the information that we track about links that are while
-- crawling a domain.

-- | The HTTP status code for a link.
data StatusCode =   NoCode 
                  | Code Int 
                  deriving (Show, Eq)

-- | A Link holds the URL being tracked, the parent page where this URL was
-- found, the status final status code and a list of prevous pages which 
-- redirected to this page.
--
-- When a URL returns a 301 or 302 status code and a location header, a new
-- Link is created with the location as the url and the old url added to the
-- previous list.
data Link = Link {
    parent :: URL        -- ^ the url where this link was found
  , previous :: [URL]    -- ^ a list of urls which redirected to this one
  , status :: StatusCode -- ^ the final status code for this url
  , theURL   :: URL      -- ^ the URL we are tracking
  } deriving (Show, Eq)

instance URLish Link where
  toURL l = (theURL l)
  toLink l = l

-- | Create a Link from a URL.
urlToLink :: URL -> Link
urlToLink url = (Link emptyURL [] NoCode url)

-- | Create a new Link from something URLish adding the old link to the
-- list of previous pages
newLinkWithPrev :: URLish a => Link -> a -> Link
newLinkWithPrev l loc = 
  (l { previous = (toURL l) : (previous l), theURL = (toURL loc)})

-- | Is the current status code 301 or 302
isRedirect :: Link -> Bool
isRedirect (Link _ _ (Code 301) _) = True
isRedirect (Link _ _ (Code 302) _) = True
isRedirect (Link _ _ _ _) = False

-- | Was the current Link the result of a redirect.
wasRedirected :: Link -> Bool
wasRedirected (Link _ (x:xs) _ _) = True
wasRedirected (Link _ _ _ _) = False

-- | Is the status code for this Link 200.
statusOk :: Link -> Bool
statusOk (Link _ _ (Code 200) _) = True
statusOk (Link _ _ _ _) = False

-- | Returns a list of distinct Links. Two links are considered the same if
-- they have the same textual representation.
distinctLinks :: [Link] -> [Link]
distinctLinks = 
  nubBy (\a b -> (u a) == (u b))
  where u = (exportURL . toURL)

-- A Script defines the crawling behavior for SiteCheck.

-- | As SiteCheck runs it can be configured to print status information using
-- the following options. When a crawl is complete any non-200 status codes
-- will be reported. If you set AllResults then all results will be shown, if
-- you set RedirectResults then all recirects are shown in addition to
-- failures.
data Option =   Limit Int          -- ^ the maximum number of pages to crawl
              | ResultFile String  -- ^ a file name where results are printed
              | PrintStatus        -- ^ print each URL as it is retrieved
              | PrintStack         -- ^ print the complete stack
              | PrintTopStack      -- ^ print the top five stack entries
              | PrintActions       -- ^ print the actions chosen for each URL
              | PrintPosts         -- ^ print each post action
              | PrintParent        -- ^ print the parent of each URL
              | AllResults         -- ^ show all results when finished
              | RedirectResults    -- ^ show errors and redirects
              deriving (Show, Eq)

-- | Predicate for selecting the Limit Option from a list of options.
isLimit :: Option -> Bool
isLimit (Limit _) = True
isLimit _         = False

-- | Get the Limit value from a list of options.
getLimit :: [Option] -> Maybe Int
getLimit (x:xs) = 
  case x of
    (Limit l) -> Just l
    _         -> getLimit xs
getLimit []   = Nothing

-- | Get the result file value from a list of options.
getResultFile :: [Option] -> Maybe String
getResultFile (x:xs) =
  case x of
    ResultFile name -> Just name
    _               -> getResultFile xs
getResultFile [] = Nothing

type Mapping a = (a -> Maybe a)

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
  | otherwise = filter (not . statusOk) links
  where redirectedOrNon200 x = wasRedirected x || (not . statusOk) x

class VisitStore a where
  addVisited :: (URLish b) => b -> StatusCode -> a -> a
  deleteVisited :: (URLish b) => b -> a -> a

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

emptyVisited :: Visited
emptyVisited = V M.empty

isVisited :: (URLish a) => Visited -> a -> Bool
isVisited (V visited) u = M.member (exportURL (toURL u)) visited

notVisited :: (URLish a) => Visited -> a -> Bool
notVisited visited u = (not (isVisited visited u))

visitedLength_ :: Visited -> Int
visitedLength_ (V v) = M.size v

exportVisited_ :: Visited -> [Link]
exportVisited_ (V v) = M.elems v

distinct :: Visited -> [Link] -> [URL] -> [URL]
distinct visited queue new = 
  filter (`notElem` queueStrings) $
  filter (notVisited visited)  new
  where queueStrings = map theURL queue

type Stack = [Link]

data State = State Visited Stack deriving (Show, Eq)

instance VisitStore State where
  addVisited l code (State visited stack) = 
    (State (addVisited l code visited) stack)

  deleteVisited l (State v s) = (State (deleteVisited l v) s)

newState :: Visited -> Stack -> State
newState v s = State v s

isNextVisited :: State -> Bool
isNextVisited (State v (x:xs)) = isVisited v x
isNextVisited (State _ []) = False

removeNext :: State -> State
removeNext (State v (x:xs)) = State v xs
removeNext state@(State _ []) = state

emptyStack :: State -> State
emptyStack (State v _) = State v []

isStackEmpty :: State -> Bool
isStackEmpty (State _ (x:xs)) = False
isStackEmpty _                = True

visitedLength :: State -> Int
visitedLength (State v _) = visitedLength_ v

exportVisited :: State -> [Link]
exportVisited (State v _) = exportVisited_ v

popStack :: State -> (Maybe Link, State)
popStack (State v (x:xs)) = (Just x, (State v xs))
popStack s@(State v []) = (Nothing, s)

pushStack :: (URLish a) => a -> State -> State
pushStack l (State v xs) = (State v ((toLink l) : xs))

stack :: State -> [Link]
stack (State _ xs) = xs

stackLength :: State -> Int
stackLength (State _ stack) = length stack

stackURLs :: State -> [URL]
stackURLs = map toURL . stack

exportStackURLs :: State -> [String]
exportStackURLs = map exportURL . stackURLs

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