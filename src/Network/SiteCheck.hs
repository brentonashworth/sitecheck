module Network.SiteCheck
  ( module Network.SiteCheck.Data
  , executeScript
  , non200Resp
  , pushLinks200
  ) where

import Network.Curl
import Control.Monad (liftM, foldM, when)
import Data.List (isSuffixOf, isPrefixOf, nub)
import Text.Regex (mkRegex, splitRegex)
import Text.Regex.Posix
import Text.HTML.TagSoup

import Network.SiteCheck.URL
import Network.SiteCheck.Data
import Network.SiteCheck.Util
import Network.SiteCheck.Output
import Network.SiteCheck.Filter

makeDecision :: Config -> Link -> [Action]
makeDecision (_, script) l =
  case safeHead $ map actions $ filter runMatch (decisions script) of
    Just x  -> x
    Nothing -> [Ignore]
  where runMatch next = (isMatch next) $ exportURL (theURL l)

getCurl :: Config -> URL -> Curl
getCurl (curls, script) url
  | isInDomain (dn script) url = intra curls
  | otherwise = extra curls

-- TODO: You shouldn't hard code ".pdf" here

useMethod :: Config -> URL -> [CurlOption]
useMethod (_, script) x
  | ".pdf" `isSuffixOf` (url_path x) = method_HEAD
  | isInDomain (dn script) x = method_GET
  | otherwise = method_HEAD

executeScript :: Script -> IO ()
executeScript script = withCurlDo $ do

  -- set up curl
  intraCurl <- initialize
  extraCurl <- initialize
  setopts intraCurl (intOpts script)
  setopts extraCurl (extOpts script)

  let curls = (Curls {intra = intraCurl, extra = extraCurl})
      config = (curls, script)

  output config =<< crawl config

-- | Takes a curl object and a root URL and returns a list of pages
-- which did not return 200 status codes. This list contains tuples with the
-- URL and status code.
crawl :: Config -> IO [Link]
crawl config = crawler config $ initState emptyVisited $ initStack config

-- TODO: The stack should never contain something that has been visited. You
-- shouldn't have to check to see if the next item in the stack has been
-- visited.

-- | Process each Link in the queue until it is empty.
crawler :: Config -> State -> IO [Link]
crawler config state
  | isStackEmpty state = return $ exportVisited state
  | isNextVisited state = crawler config $ tailStack state -- defensive (bad)
  | isOverLimit config state = crawler config $ deleteStack state
  | otherwise = do
      whenOpt config PrintStack $ printStack state
      whenOpt config PrintTopStack $ printTopStack state
      crawler config =<< processNext config state

processNext :: Config -> State -> IO State
processNext config s = 
  let (x, state) = popStack s in
  case x of
    Nothing   -> return state
    Just link -> do
      let actions = makeDecision config link
          url = toURL link
          when' = whenOpt config
      when' PrintStatus $ printStatus state url
      r <- doCurl config url
      case r of
        Error -> do
          when' PrintStatus $ printError url
          return $ addVisited link (Code 9999) state
        Response code _ _ -> do
          when' PrintStatus $ putStrLn (show code)
          when' PrintParent $ printParent link
          when' PrintActions $ printActions actions
          s <- foldM (runAction config link r) state actions
          return $ addVisited link (Code code) s

runAction :: Config -> Link -> Response -> State -> Action -> IO State
runAction config@(_, script) l r state action = do
  case action of
    Ignore          -> return state
    PushLinks       -> return $ pushLinksAction script l r state
    post@(Post _ _) -> postFormAction config l r post state
    get@(Get _ _)   -> return $ getAction get state

pushLinksAction :: Script -> Link -> Response -> State -> State
pushLinksAction script l r state
  | r == Error = state
  | rStatus r == 200 = pushLinks200 script l r state
  | otherwise = non200Resp l r state

postFormAction :: Config -> Link -> Response -> Action -> State -> IO State
postFormAction config l r (Post url params) state =
  let opts = method_POST ++ 
             [CurlPostFields (map (\(a, b) -> (a++"="++b)) params)]
      curl = (getCurl config (theURL l))
  in
    do
      whenOpt config PrintPosts $ printPost url opts
      r' <- do_curl_ curl url opts :: IO CurlResponse
      case (respStatus r') of
        301 -> return $ followPost l r' state
        302 -> return $ followPost l r' state
        _   -> return state
postFormAction _ _ _ _ state = return state

getAction :: Action -> State -> State
getAction (Get url params) state = 
  let params' = map encode params in
    case importURL url of
      Just u  -> pushStack (foldl add_param u params') state
      Nothing -> state
  where encode (a, b) = (a, encString True ok_param b) 
getAction _ state = state

pushLinks200 :: Script -> Link -> Response -> State -> State
pushLinks200 script link r = mergeWithStack link (extractLinks script r link)

non200Resp :: Link -> Response -> State -> State
non200Resp l r state
  | r == Error = state
  | rStatus r == 302 = redirect l r state
  | rStatus r == 301 = redirect l r state
  | otherwise = state

followPost :: Link -> CurlResponse -> State -> State
followPost l r state = 
  case fullLocation l (toResponse r) of
    Just loc -> pushStack loc . deleteVisited loc $ state
    Nothing  -> state

redirect :: Link -> Response -> State -> State
redirect l r state = 
    case fullLocation l r of
      Just loc -> if loc `elem` (previous l)
                  then state
                  else addRedirect loc l state
      Nothing -> state

doCurl :: Config -> URL -> IO Response
doCurl config url = do
  r <- do_curl_ (getCurl config url) (exportURL url) (useMethod config url)
  return $ toResponse r

fullLocation :: Link -> Response -> Maybe URL
fullLocation l r = fmap (makeAbsolute (toURL l)) $ 
                   importURL =<< header r "Location"

addRedirect :: URL -> Link -> State -> State
addRedirect loc l = pushStack (newLinkWithPrev l loc)

toResponse :: CurlResponse -> Response
toResponse r =
  if respCurlCode r == CurlOK
  then Response (respStatus r) (respHeaders r) (respBody r)
  else Error

extractLinks :: (URLish a) => Script -> Response -> a -> [URL]
extractLinks script r u =
  let url = toURL u in
  case header r "Content-Type" of
    Just x -> if x =~ "text/html"
              then (filterLinks (mappings script) url) $ 
                   linkify (dn script) url (rBody r)
              else []
    Nothing -> []

-- | Get the links on a page only if the page is within our domain. If not
-- then return an empty list.
linkify :: String -> URL -> String -> [String]
linkify d url l = if isInDomain d url
                then [x | TagOpen "a" atts <- parseTags l, ("href", x) <- atts]
                else [] 
