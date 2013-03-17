module Data.String.Extra where

import Data.Char

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
