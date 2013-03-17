-- | Display times as a relative duration. E.g. x days ago.

module Data.Time.Relative where

import Data.List
import Data.Time
import Text.Printf

-- | Display a time span as one time relative to another.
relativeZoned :: ZonedTime -- ^ The later time span.
              -> ZonedTime -- ^ The earlier time span.
              -> Bool      -- ^ Display 'in/ago'?
              -> String    -- ^ Example: '3 seconds ago', 'in three days'.
relativeZoned t1 t2 fix =
  relative (zonedTimeToUTC t1) (zonedTimeToUTC t2) fix

-- | Display a time span as one time relative to another.
relative :: UTCTime -- ^ The later time span.
         -> UTCTime -- ^ The earlier time span.
         -> Bool    -- ^ Display 'in/ago'?
         -> String  -- ^ Example: '3 seconds ago', 'in three days'.
relative t1 t2 fix = maybe "unknown" format $ find (\(s,_,_) -> abs span>=s) $ reverse ranges where
  minute = 60; hour = minute * 60; day = hour * 24;
  week = day * 7; month = day * 30; year = month * 12
  format range =
    (if fix && span>0 then "in " else "")
    ++ case range of
        (_,str,0) -> str
        (_,str,base) -> printf str (abs $ round (span / base) :: Integer)
    ++ (if fix && span<0 then " ago" else "")
  span = t1 `diffUTCTime` t2
  ranges = [(0,"%d seconds",1)
           ,(minute,"a minute",0)
           ,(minute*2,"%d minutes",minute)
           ,(minute*30,"half an hour",0)
           ,(minute*31,"%d minutes",minute)
           ,(hour,"an hour",0)
           ,(hour*2,"%d hours",hour)
           ,(hour*3,"a few hours",0)
           ,(hour*4,"%d hours",hour)
           ,(day,"a day",0)
           ,(day*2,"%d days",day)
           ,(week,"a week",0)
           ,(week*2,"%d weeks",week)
           ,(month,"a month",0)
           ,(month*2,"%d months",month)
           ,(year,"a year",0)
           ,(year*2,"%d years",year)
           ]
