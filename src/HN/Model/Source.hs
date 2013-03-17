module HN.Model.Source where

import HN.Types

-- | Generate a unique ID for the given source type.
sourceId :: Source -> Int
sourceId HaskellReddit = 1
