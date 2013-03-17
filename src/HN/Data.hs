module HN.Data
  (module Data.List
  ,module Data.Time
  ,module Data.Maybe
  ,module Data.String.Extra
  ,module Data.Char
  ,module Data.Default
  ,module Data.Pagination
  ,module Data.String
  ,module Data.Ord
  ,fi
  )
  where

import Data.List hiding (head,span)
import Data.Time
import Data.Maybe
import Data.String.Extra
import Data.Char
import Data.Default
import Data.Pagination
import Data.String
import Data.Ord (comparing)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
