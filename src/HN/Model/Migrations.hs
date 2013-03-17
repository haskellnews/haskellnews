{-# LANGUAGE OverloadedStrings #-}

-- | The server's database migrations.

module HN.Model.Migrations where

import GHC.Int
import Snap.App

-- | Migrations.
versions :: [(Int,Model c s Int64)]
versions = zip [1..] ms where
  ms = [ex ["CREATE TABLE source"
           ,"(id serial primary key"
           ,",title text not null"
           ,");"
           ,""
           ,"CREATE TABLE item "
           ,"(id serial primary key"
           ,",source integer not null references source(id) on delete cascade on update cascade"
           ,",added timestamp with time zone not null default now()"
           ,",published timestamp with time zone not null"
           ,",title text not null"
           ,",description text not null"
           ,");"]
       ]
  ex q = exec q ()
