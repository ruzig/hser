#!/usr/bin/env stack
-- stack script --resolver lts-8.22
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
    ( displayUsers
    ) where

import qualified Data.ByteString.Lazy as B

import qualified Data.Map as Map

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.Aeson
import           Data.List
import           Data.Text (Text)
import           Network.HTTP.Conduit (simpleHttp)
import GHC.Generics



data Guser = Guser { login :: !String
                    , html_url :: !String
										} deriving (Generic, Show)

instance FromJSON Guser
instance ToJSON Guser

data Gsearch = Gsearch { items :: [Guser] } deriving (Generic, Show)
instance FromJSON Gsearch
instance ToJSON Gsearch

renderGuser :: Guser -> String
renderGuser u = "UserName: " ++ name ++ " --url: " ++ profileUrl
  where
    name :: String
    name = login u
    profileUrl :: String
    profileUrl = html_url u

renderGsearch :: Gsearch -> String
renderGsearch g = unwords $ map renderGuser $ items g

jsonURL :: String
jsonURL = "https://api.github.com/search/users?q=location:singapore"

getJSON :: IO B.ByteString
getJSON = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  initRequest <- parseRequest jsonURL
  let request = initRequest { requestHeaders =
				[ ("Content-Type", "application/json; charset=utf-8")
				  , ("User-Agent", "hser")
				]
		}
  res <- httpLbs request manager
  return $ responseBody res


displayUsers :: IO ()
displayUsers = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String Gsearch)
  case d of
    Left err -> putStrLn err
    Right ps -> print $ renderGsearch ps

