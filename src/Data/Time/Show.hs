{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Date/time showing functions.

module Data.Time.Show
  (showDateTime)
  where
  
import Data.Time     (FormatTime,formatTime)
import System.Locale (defaultTimeLocale)

showDateTime :: FormatTime t => t -> String
showDateTime time = formatTime defaultTimeLocale "%F %T %Z" time
