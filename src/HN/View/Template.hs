{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HN.View.Template where

import HN.View
import qualified Text.Blaze.Html5 as H (title)
import qualified Text.Blaze.Html5.Attributes as A (name)

template name innerhead innerbody = do
  docType
  html $ do
    head $ do H.title "Haskell News"
              link ! rel "alternate"  ! type_ "application/rss+xml" ! title "Haskell News Feed" ! href "/feed"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap-responsive.css"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/haskellnews.css"
              meta ! A.name "viewport" ! content "width=device-width, user-scalable=no"
              meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
              innerhead
              meta ! A.name "description" ! content "An aggregation of all online content related to Haskell, including Google+, Reddit, Twitter, GitHub, HaskellWiki, Stack Overflow, Planet Haskell, Hackage, ..."
    body !# name $ do
      innerbody
      footer
      script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ mempty
      script ! src "/js/haskellnews.js" $ mempty
      preEscapedToHtml ("<script type=\"text/javascript\"> var _gaq = _gaq \
                       \|| []; _gaq.push(['_setAccount', 'UA-39372380-1']);\
                       \ _gaq.push(['_trackPageview']); (function() {var ga\
                       \ = document.createElement('script'); ga.type = 'tex\
                       \t/javascript'; ga.async = true; ga.src = ('https:' \
                       \== document.location.protocol ? 'https://ssl' : \
                       \'http://www') + '.google-analytics.com/ga.js'; var\
                       \ s = document.getElementsByTagName('script')[0]; \
                       \s.parentNode.insertBefore(ga, s);})(); </script>" :: String)

showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest

footer =
  div !# "footer" $
    div !. "container" $ do
      p !. "muted credit" $ do
        a ! href "http://haskellnews.org" $ "Haskell News"
        " by "
        a ! href "http://chrisdone.com" $ "Chris Done"
        " | "
        a ! href "https://github.com/chrisdone/haskellnews" $ "Source code"
        " | "
        a ! href "http://haskell.org/" $ "Haskell"
