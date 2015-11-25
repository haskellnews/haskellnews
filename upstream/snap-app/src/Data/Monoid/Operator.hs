-- | Useful operator (++) = mappend.

module Data.Monoid.Operator where

import Data.Monoid (Monoid)
import Data.Monoid (mappend)
import Prelude()

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++
