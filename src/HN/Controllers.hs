{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import Snap.App

home :: Controller c s ()
home = writeText "Hello, World!"
