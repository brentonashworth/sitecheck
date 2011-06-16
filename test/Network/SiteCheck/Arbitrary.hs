module Network.SiteCheck.Arbitrary where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List (intersperse, nub, nubBy)
import Network.URL

import Network.SiteCheck.Data
import Network.SiteCheck.Util

hostNameSource = (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ".-")
paramSource = hostNameSource ++ " ~;:@$_!*'(),"

webString :: Gen String
webString = listOf $ elements (paramSource ++ "?/=&^")

genPath :: Gen String
genPath = do
  x <- listOf $ webString
  x' <- return (map (encString True ok_path) x)
  return $ concat $ intersperse "/" x'

genParams :: Gen [(String, String)]
genParams = do
  n <- listOf $ webString
  v <- listOf $ webString
  return $ nubBy (\a b -> (fst a) == (fst b)) $ zip (enc n) (enc v)
  where enc x =  map (encString True ok_param) x

instance Arbitrary URL where
  arbitrary = do
    urlType <- arbitrary
    path <- genPath
    params <- genParams
    return (URL urlType path params)

instance Arbitrary URLType where
  arbitrary = do
    n <- choose (1,3) :: Gen Int
    case n of
      1 -> do x <- arbitrary
              return (Absolute x)
      2 -> return HostRelative
      3 -> return PathRelative

hostString :: Gen String
hostString = listOf $ elements hostNameSource

instance Arbitrary Host where
  arbitrary = do
    prot <- arbitrary
    host <- hostString
    port <- elements [Just 80, Just 8080, Nothing]
    return (Host prot host port)

instance Arbitrary Protocol where
  arbitrary = do
    n <- choose (1,3) :: Gen Int
    case n of
      1 -> do x <- arbitrary
              return (HTTP x)
      2 -> do x <- arbitrary
              return (FTP x)
      3 -> return (RawProt "some")

instance Arbitrary StatusCode where
  arbitrary = elements [Code 200, Code 301, Code 302, Code 500, NoCode] 

relativeURLs :: [String]
relativeURLs = [ "?name=ken"
               , "/page?age=23"
               , "/resouce?age=23#something"
               , "/menu"
               , "/page#something"
               ]

fullURLs :: [String]
fullURLs = [ "http://www.host.com/some/other/thing?age=23"
           , "http://www.company.com/"
           , "http://www.company.com/cool"
           , "https://james.org/home/page"
           , "http://ibm.com/help/resources.html?item=6"
           , "http://formpluslogic.com:8080/demo?item=6"
           ]

relURL :: Gen URL
relURL = elements $ fromMaybe $ map importURL relativeURLs

fullURL :: Gen URL
fullURL = elements $ fromMaybe $ map importURL fullURLs

anyURL :: Gen URL
anyURL = elements $ fromMaybe $ map importURL $ concat [relativeURLs, fullURLs]

instance Arbitrary Visited where
  arbitrary = do
    urls <- listOf fullURL
    status <- arbitrary
    let v = foldr (\next acc -> addVisited next status acc) emptyVisited urls
    elements [emptyVisited, v]

instance Arbitrary Link where
  arbitrary = do
    code <- arbitrary
    parent <- fullURL
    prev <- listOf fullURL
    link <- fullURL
    let prev' = filter (\x -> link /= x ) prev
    return (Link parent prev' code link)

instance Arbitrary State where
  arbitrary = do
    vis <- arbitrary
    links <- listOf $ arbitrary
    let links' = distinctLinks links
        vis' = foldr (\next acc -> deleteVisited next acc) vis links'
    return $ initState vis' links'

instance Arbitrary Response where
  arbitrary = do
    r <- relURL
    let rel = exportURL r
    elements [ Error
             , (Response 200 [] "")
             , (Response 200 [] ("<a href='"++ rel ++"'>hello</a>"))
             , (Response 301 [("Location", rel)] "")
             , (Response 302 [("Location", rel)] "")
             , (Response 500 [] "")
             ]

instance Show Decision where
  show x = "desision to " ++ (show (actions x))

instance Show Script where
  show x = "script for " ++ (dn x)

instance Arbitrary Script where
  arbitrary = return (Script { dn = "www.host.com"
                             , options = []
                             , intOpts = []
                             , extOpts = []
                             , seed = []
                             , decisions = []
                             , mappings = (Mappings [] [] [])
                             })
