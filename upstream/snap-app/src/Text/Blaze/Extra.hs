{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Text.Blaze.Extra where

import Control.Monad
import Data.Monoid
import Data.Monoid.Operator
import Prelude                     hiding ((++),head,div)
import Text.Blaze.Html5            as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal         (Attributable)
import Network.URI.Params
import Network.URI
import Text.Printf
import Data.List (intercalate)

(!.) :: (Attributable h) => h -> AttributeValue -> h
elem !. className = elem ! class_ className

(!#) :: (Attributable h) => h -> AttributeValue -> h
elem !# idName = elem ! A.id idName

linesToHtml :: String -> Html
linesToHtml str = forM_ (lines str) $ \line -> do toHtml line; br

htmlIntercalate :: Html -> [Html] -> Html
htmlIntercalate _ [x] = x
htmlIntercalate sep (x:xs) = do x; sep; htmlIntercalate sep xs
htmlIntercalate _ []  = mempty

htmlCommasAnd :: [Html] -> Html
htmlCommasAnd [x] = x
htmlCommasAnd [x,y] = do x; " and "; y
htmlCommasAnd (x:xs) = do x; ", "; htmlCommasAnd xs
htmlCommasAnd []  = mempty

htmlCommas :: [Html] -> Html
htmlCommas = htmlIntercalate ", "

hrefSet :: URI -> String -> String -> Attribute
hrefSet uri key value = hrefURI updated where
  updated = updateUrlParam key value uri

hrefURI :: URI -> Attribute
hrefURI uri = href (toValue (showURI uri)) where
  showURI URI{..} = uriPath ++ uriQuery

hrefURIWithHash :: URI -> String -> Attribute
hrefURIWithHash uri hash = href (toValue (showURI uri ++ "#" ++ hash)) where
  showURI URI{..} = uriPath ++ uriQuery

hrefAssoc :: String -> [(String,String)] -> Attribute
hrefAssoc path qs = href (toValue uri) where
  uri = "/" ++ path ++ "?" ++ intercalate "&" (map (uncurry (printf "%s=%s")) qs)

instance ToValue URI where
  toValue = toValue . show
