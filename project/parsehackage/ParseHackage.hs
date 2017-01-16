{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Very simple parser for hackage

module ParseHackage where

import           Data.Aeson           (FromJSON (..), decodeStrict)
import           Data.Aeson.TH        (Options (..), defaultOptions, deriveFromJSON)
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import           Network.HTTP         (HeaderName (..), Request (..), RequestMethod (..),
                                       getRequest, getResponseBody, mkHeader, simpleHTTP)
import qualified Network.HTTP         as HTTP
import           Network.URI          (parseURI)

import           ParserExtra          (customOptions)

newtype Package = Package { package_packageName :: Text } deriving Show

deriveFromJSON customOptions ''Package


getRequestJSON :: String -> Request String
getRequestJSON prefix =
    Request
    { rqURI = fromMaybe (error "getRequestJson") $
      parseURI $ "http://hackage.haskell.org/" ++ prefix
    , rqMethod = GET
    , rqHeaders = [mkHeader HdrAccept "application/json"]
    , rqBody = ""
    }

jp s = error ("couldn't parse " ++ s)

getResponseBodyT = fmap T.pack . getResponseBody
getResponseBodyBS = fmap encodeUtf8 . getResponseBodyT
getResponseBodyData endpoint = do
    req <- simpleHTTP (getRequestJSON endpoint)
    body <- getResponseBodyBS req
    pure $ fromMaybe (jp endpoint) . decodeStrict $ body

main = do
    (packages :: [Package]) <- getResponseBodyData "packages/"
    print $ take 5 packages
