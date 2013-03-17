{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Instances that can be converted to a string.

module Data.String.ToString where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8 (toString)
import           Data.Char

class ToString string where
  toString :: string -> String

instance ToString String where toString = id

(+++) :: (ToString str1,ToString str2) => str1 -> str2 -> String
str1 +++ str2 = toString str1 ++ toString str2

instance ToString ByteString where toString = UTF8.toString
