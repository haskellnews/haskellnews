{-# LANGUAGE OverloadedStrings #-}

-- | The server's database migrations.

module HN.Model.Migrations where

import GHC.Int
import Snap.App

-- | Migrations.
versions :: [(Int,Model c s Int64)]
versions = zip [1..] ms where
  ms = []
