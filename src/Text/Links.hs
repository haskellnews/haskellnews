{-# LANGUAGE OverloadedStrings #-}

-- | Split some text into links and text.

module Text.Links
 (explodeLinks)
  where

import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI
import           Test.HUnit

explodeLinks :: Text -> [Either URI Text]
explodeLinks = consume where
  consume t =
    if T.null t
       then []
       else case T.breakOn prefix t of
              (before,"") -> [Right t]
              (before,after) ->
                case T.span allowed after of
                  (murl,rest) -> case parseURI (T.unpack murl) of
                    Nothing -> let leading = before <> prefix
                               in case consume (T.drop 4 after) of
                                    (Right x:xs) -> Right (leading <> x) : xs
                                    xs -> Right leading : xs
                    Just uri -> (if T.null before then id else (Right before :))
                                (Left uri : explodeLinks rest)
  prefix = "http"
  -- Because it's not normal, and it's annoying.
  allowed '(' = False
  allowed ')' = False
  allowed c = isAllowedInURI c

tests :: Test
tests = TestList $ map testify
  [("empty","",[])
  ,("just text","abc",[Right "abc"])
  ,("just link","http://abc",[Left (uri "http://abc")])
  ,("link start","http://abc foobar",[Left (uri "http://abc"),Right " foobar"])
  ,("link end","foobar http://abc",[Right "foobar ",Left (uri "http://abc")])
  ,("link mid","foobar http://abc zot",[Right "foobar ",Left (uri "http://abc"),Right " zot"])
  ,("has http","http http://abc zot",[Right "http ",Left (uri "http://abc"),Right " zot"])
  ,("has http (2)","foo http http://abc zot",[Right "foo http ",Left (uri "http://abc"),Right " zot"])
  ,("non-uri char","foo \"http://abc\" zot",[Right "foo \"",Left (uri "http://abc"),Right "\" zot"])
  ,("non-uri char (2)","foo <http://abc> zot",[Right "foo <",Left (uri "http://abc"),Right "> zot"])
  ]

  where uri = fromJust . parseURI
        testify (label,param,expected) = TestCase (assertEqual label (explodeLinks param) expected)
