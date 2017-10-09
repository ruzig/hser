#!/usr/bin/env stack
-- stack script --resolver lts-8.22
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( displayUsers
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.Aeson.Parser           (json)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)

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
	withResponse request manager $ \response -> do
		value <- bodyReaderSource (responseBody response)
					$$ sinkParser json
		print value

