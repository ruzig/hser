#!/usr/bin/env stack
-- stack script --resolver lts-8.22
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( displayUsers
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

displayUsers :: IO ()
displayUsers = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
    setGlobalManager manager

    initialRequest <- parseRequest "https://api.github.com/search/users?q=location:singapore"
    let request = initialRequest
            { method = "GET"
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8"),
                ("User-Agent", "hser")
                ]
            }
    response <- httpLBS request

    L8.putStrLn $ getResponseBody response

