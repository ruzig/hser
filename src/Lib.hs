#!/usr/bin/env stack
-- stack script --resolver lts-8.22
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( displayUsers
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.Aeson
import           Data.Text (Text)


data Guser = Guser { login :: !Text
											, avatar_url :: !Text
										}
data Gsearch = Gsearch { items :: [Guser] }

initRequest :: String -> Request
initRequest url = do
	initialRequest <- parseRequest 
	return initialRequest
		{ method = "GET"
			, requestHeaders =
				[ ("Content-Type", "application/json; charset=utf-8")
				  , ("User-Agent", "hser")
				]
		}


fetchUser :: String -> IO (Either String Gsearch)
fetchUser url = do
	manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
	setGlobalManager manager
	request <- initRequest(url)
	withResponse request manager $ \res -> do
		return $ eitherDecode $ responseBody res

displayUsers :: IO ()
displayUsers = do
  value <- fetchUser url
  case value of
    Left err -> putStrLn err
    Right gs -> print gs
  where
    url = "https://api.github.com/search/users?q=location:singapore"

