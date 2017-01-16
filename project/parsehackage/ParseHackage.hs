{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Very simple parser for hackage

module ParseHackage where

import           Control.Monad                         (forM_, replicateM, when)
import           Data.Aeson                            (FromJSON (..), decodeStrict)
import           Data.Aeson.TH                         (Options (..), defaultOptions,
                                                        deriveFromJSON)
import           Data.Aeson.Types                      (typeMismatch)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.List                             (isSuffixOf)
import           Data.Maybe                            (fromMaybe)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Time                             (UTCTime)
import           Data.Time.Format                      (parseTimeM)
import           Data.Version                          (Version (..), parseVersion)
import           Distribution.License                  (License (..))
import           Distribution.PackageDescription       (GenericPackageDescription (..),
                                                        PackageDescription (..))
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Network.HTTP                          (HeaderName (..), Request (..),
                                                        RequestMethod (..), getRequest,
                                                        getResponseBody, mkHeader,
                                                        simpleHTTP)
import qualified Network.HTTP                          as HTTP
import           Network.URI                           (parseURI)
import           System.Random                         (randomRIO)
import           Text.ParserCombinators.ReadP          (readP_to_S)

import           ParserExtra

newtype Package = Package { package_packageName :: Text } deriving Show
newtype Versions = Versions { versions_normal_version :: [String] } deriving Show
data User = User
    { user_username :: Text
    , user_userid   :: Integer
    } deriving (Show)
newtype Maintainers = Maintainers { maintainers_members :: [User] } deriving Show

deriveFromJSON customOptions ''Package
deriveFromJSON customOptions ''Versions
deriveFromJSON customOptions ''User
deriveFromJSON customOptions ''Maintainers

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
getResponseBodyDataMaybe endpoint = do
    putStrLn endpoint
    req <- simpleHTTP (getRequestJSON endpoint)
    decodeStrict <$> getResponseBodyBS req
getResponseBodyData endpoint = do
    fromMaybe (jp endpoint) <$> getResponseBodyDataMaybe endpoint

parseCabal x =
    case parsePackageDescription x of
        ParseFailed _ -> Nothing
        ParseOk [] GenericPackageDescription {..} ->
            let PackageDescription {..} = packageDescription
            in Just $
               ( T.pack $ show license
               , T.pack homepage
               , T.pack author
               , T.pack category
               , buildDepends)

data OutputUser = OutputUser
    { ouLogin    :: Text
    , ouName     :: Text
    , ouEmail    :: Text
    , ouPassHash :: Text
    , ouGpgKey   :: Maybe (Text, Text) -- (pkid28, sig44)
    , ouSshKeys  :: [Text] -- Text
    }

data OutputVersion = OutputVersion
    { ovVersion  :: Version
    , ovCategory :: Text
    , ovUploaded :: UTCTime
    , ovUploader :: Text
    , owSource   :: Text
    } deriving (Show)

data OutputPackage = OutputPackage
    { oPackageName :: Text
    , oSite        :: Text
    , oLicense     :: Text
    , oAuthor      :: Text
    , oVersions    :: [OutputVersion]
    } deriving Show

main = do
    putStrLn "parsing users..."
    (users :: [User]) <- getResponseBodyData "users/"
    outputUsers <- flip mapM (users `zip` allNamesSurnames) $ \((User ouLogin _), ouName) -> do
        ouEmail <- randomEmail ouLogin
        ouPassHash <- randomB64 32
        ouGpgKey <- do
            rand <- randomRIO (0::Int,15)
            if rand == 0
            then Just <$> ((,) <$> randomB64 20 <*> randomB64 32)
            else pure Nothing
        ouSshKeys <- do
            num <- randomRIO (0::Int, 3)
            replicateM num $ randomSSH ouEmail
        pure $ OutputUser{..}
    print $ take 5 users
    putStrLn "parsing packages..."
    (packages :: [Package]) <- take 20 <$> getResponseBodyData "packages/"
    putStrLn "parsing stuff..."
    withVersions <- flip mapM (take 5 packages) $ \p@(Package pname) -> do
        (Versions versions) <-
            getResponseBodyData $ "package/" ++ T.unpack pname ++ "/preferred"
        let (versions' :: [Version]) =
                map (\v -> fst $ head $ readP_to_S parseVersion v) versions
        print $ take 5 versions'

        (Maintainers maintainers) <-
            getResponseBodyData $ "package/" ++ T.unpack pname ++ "/maintainers/"
        let author = head maintainers
        print $ take 5 maintainers

        outputVersions <- flip mapM versions' $ \version -> do
            undefined
        undefined
    print $ take 5 packages
