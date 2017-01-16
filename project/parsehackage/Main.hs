{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

-- | Very simple parser for hackage

module Main where


import           Control.Lens                          (view, _1, _2, _3, _4)
import           Control.Monad                         (forM_, guard, replicateM, when)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Maybe             (MaybeT (..), runMaybeT)
import           Data.Aeson                            (FromJSON (..),
                                                        Value (Object, String),
                                                        decodeStrict, (.!=), (.:), (.:?))
import           Data.Aeson.TH                         (Options (..), defaultOptions,
                                                        deriveFromJSON)
import           Data.Aeson.Types                      (typeMismatch)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.List                             (isSuffixOf, nub)
import           Data.Map                              ((!))
import qualified Data.Map                              as M
import           Data.Maybe                            (catMaybes, fromJust, fromMaybe,
                                                        isJust)
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import qualified Data.Text.IO                          as TIO
import           Data.Time                             (UTCTime)
import           Data.Time.Format                      (defaultTimeLocale, parseTimeM)
import           Data.Version                          (Version (..), parseVersion,
                                                        showVersion)
import           Distribution.License                  (License (..))
import           Distribution.Package                  (Dependency (..), unPackageName)
import           Distribution.PackageDescription       (CondTree (..),
                                                        GenericPackageDescription (..),
                                                        PackageDescription (..))
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Distribution.Version                  (VersionRange (..))
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
    , user_userid   :: Int
    } deriving (Show)
newtype Maintainers = Maintainers { maintainers_members :: [User] } deriving Show
newtype UTCTimeW = UTCTimeW { getUTCTimeW :: UTCTime } deriving Show

deriveFromJSON customOptions ''Package
deriveFromJSON customOptions ''Versions
deriveFromJSON customOptions ''User
deriveFromJSON customOptions ''Maintainers

parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime s =
    parseTimeM True defaultTimeLocale "%a %b %e %T %Z %Y" (T.unpack s)

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
getResponseBodyDataRaw endpoint = do
    putStrLn endpoint
    req <- simpleHTTP (getRequestJSON endpoint)
    body <- getResponseBodyBS req
    pure body
getResponseBodyDataMaybe endpoint = do
    decodeStrict <$> getResponseBodyDataRaw endpoint
getResponseBodyData endpoint = do
    fromMaybe (jp endpoint) <$> getResponseBodyDataMaybe endpoint

fromCondTree :: CondTree a b c -> [b]
fromCondTree CondNode{..} =
    condTreeConstraints : concatMap (fromCondTree . view _2) condTreeComponents

anyDep :: VersionRange -> [Version]
anyDep AnyVersion                      = [Version [0,0,1] []]
anyDep (ThisVersion v)                 = [v]
anyDep (LaterVersion v)                = [v]
anyDep (EarlierVersion v)              = [v]
anyDep (WildcardVersion v)             = [v]
anyDep (UnionVersionRanges vr vr')     = anyDep vr ++ anyDep vr'
anyDep (IntersectVersionRanges vr vr') = anyDep vr ++ anyDep vr'
anyDep (VersionRangeParens vr)         = anyDep vr

parseCabal x =
    case parsePackageDescription x of
        ParseFailed _ -> Nothing
        ParseOk [] GenericPackageDescription {..} ->
            let PackageDescription {..} = packageDescription
                libDeps = concat $ maybe [] fromCondTree condLibrary
                libExe = concat $ concatMap (fromCondTree . snd) condExecutables
                libTest = concat $ concatMap (fromCondTree . snd) condTestSuites
            in Just $
               ( T.pack $ show license
               , T.pack homepage
               , T.pack author
               , T.pack category
               , concatMap (\(Dependency p vr) -> map (T.pack $ unPackageName p,) $ anyDep vr) $
                 libDeps ++ libExe ++ libTest)

----------------------------------------------------------------------------
-- Output datatypes
----------------------------------------------------------------------------

data OutputUser = OutputUser
    { ouLogin    :: Text
    , ouId       :: Int
    , ouName     :: Text
    , ouEmail    :: Text
    , ouPassHash :: Text
    , ouGpgKey   :: Maybe (Text, Text) -- (pkid28, sig44)
    , ouSshKeys  :: [Text] -- Text
    } deriving (Show, Eq, Ord)

data OutputVersion = OutputVersion
    { ovVersion  :: Version
    , ovCategory :: Text
    , ovUploaded :: UTCTime
    , ovUploader :: Text
    , ovDeps     :: [(Text, Version)]
    } deriving (Show, Eq, Ord)

data OutputPackage = OutputPackage
    { oPackageName       :: Text
    , oSite              :: Text
    , oLicense           :: Text
    , oAuthor            :: Text
    , oVersions          :: [OutputVersion]
    , oNotifyUploader    :: Bool
    , oNotifyMaintainers :: Bool
    , oDeprecated        :: Bool
    , oPrivate           :: Bool
    } deriving (Show,Eq,Ord)

----------------------------------------------------------------------------
-- SQL generation
----------------------------------------------------------------------------

insertUsers :: [OutputUser] -> IO ()
insertUsers !users = do
    putStrLn "dumping users"
    let pre1 = "INSERT INTO RegisteredUser VALUES\n"
        row1 OutputUser{..} =
            mconcat ["  (", T.pack (show ouId), ", '", ouLogin
                    ,"', '", ouName, "', '", ouEmail, "',"
                    , "decode('", ouPassHash, "', 'base64'))"]
        insertUsers = pre1 <> T.intercalate ",\n" (map row1 users) <> ";"
    TIO.writeFile "realdata_users.sql" insertUsers

    putStrLn "dumping ssh"
    let usersWithSsh = filter (not . null . ouSshKeys) users
        pre2 = "INSERT INTO SshKey (SshPubKey, SshOwner) VALUES\n"
        row2 OutputUser{..} =
            map
            (\sshKey -> mconcat ["  ('", sshKey, "', ", T.pack (show ouId), ")"])
            ouSshKeys
        insertSsh = pre2 <> T.intercalate ",\n" (concatMap row2 usersWithSsh) <> ";"
    TIO.writeFile "realdata_ssh.sql" insertSsh

    putStrLn "dumping gpg"
    let usersWithGpg = filter (isJust . ouGpgKey) users
        pre3 = "INSERT INTO GpgKey VALUES\n"
        row3 OutputUser{..} =
            (\(gpk,psig) -> mconcat
              ["  (", T.pack (show ouId), ", decode('", gpk, "', 'base64'), decode('",
               psig, "', 'base64'))"])
            (fromJust ouGpgKey)
        insertGpg = pre3 <> T.intercalate ",\n" (map row3 usersWithGpg) <> ";"
    TIO.writeFile "realdata_gpg.sql" insertGpg
    putStrLn "dumping done"

insertPackages :: [OutputPackage] -> IO ()
insertPackages = undefined

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

normalizePackages :: [OutputPackage] -> IO [OutputPackage]
normalizePackages packages = do
    let packageMap =
            M.fromList $ map (\p -> (oPackageName p, map ovVersion $ oVersions p)) packages
        randomDepP p = do
            let versions = packageMap ! p
            r <- randomRIO (0, length versions - 1)
            pure $ versions !! r
    flip mapM packages $ \op@OutputPackage{..} -> do
        newVersions <- flip mapM oVersions $ \ov@OutputVersion{..} -> do
            newDependencies <- flip mapM ovDeps $ \d@(dep,version) -> do
                let depEx = dep `M.member` packageMap
                let depVersEx = version `elem` (packageMap ! dep)
                if | not depEx -> pure Nothing
                   | not depVersEx -> Just . (dep,) <$> randomDepP dep
                   | otherwise -> pure $ Just d
            pure $ ov { ovDeps = catMaybes newDependencies }
        pure $ op { oVersions = newVersions }

main = do
    putStrLn "parsing users..."
    (users :: [User]) <- getResponseBodyData "users/"
    outputUsers <- flip mapM (users `zip` allNamesSurnames) $ \((User ouLogin ouId), ouName) -> do
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
    insertUsers outputUsers

    putStrLn "parsing packages..."
    --(packages :: [Package]) <- getResponseBodyData "packages/"
    let packages = map Package topHackagePackages
    putStrLn $ "total packages on hackage: " ++ show (length packages)
    outputPackages <- flip mapM (take 20 packages) $ \p@(Package oPackageName) -> runMaybeT $ do
        let pname = T.unpack oPackageName
        (Versions versions) <-
            lift $ getResponseBodyData $ "package/" ++ pname ++ "/preferred"
        let takeLast n xs = drop (length xs - n) xs
        let (versions' :: [Version]) =
                takeLast 3 $
                map (\v -> fst $ last $ readP_to_S parseVersion v) versions

        (Maintainers maintainers) <-
            lift $ getResponseBodyData $ "package/" ++ pname ++ "/maintainers/"

        outputVersions <- flip mapM versions' $ \ovVersion -> lift $ runMaybeT $ do
            let versS = showVersion ovVersion
                urlPrefix = "package/" ++ pname ++ "-" ++ versS ++ "/"
            uploadTimeRaw <-
                lift $
                getResponseBodyT =<<
                simpleHTTP (getRequestJSON $ urlPrefix ++ "upload-time")
--            lift $ print version
--            lift $ print versS
--            lift $ print urlPrefix
--            lift $ print uploadTimeRaw
            ovUploaded <- MaybeT $ pure $ parseUTCTime uploadTimeRaw

            ovUploader <-
                lift $
                getResponseBodyT =<<
                simpleHTTP (getRequestJSON $ urlPrefix ++ "uploader")

            cabalFile <-
                lift $
                getResponseBodyT =<<
                simpleHTTP (getRequestJSON $ urlPrefix ++ pname ++ ".cabal")
            cabal@(ovLicense, ovHomepage, ovAuthor, ovCategory, ovDeps) <-
                MaybeT $ pure $ parseCabal $ T.unpack cabalFile
            pure $ (OutputVersion{..}, ovAuthor, ovHomepage, ovLicense)
        let outputVersions' = catMaybes outputVersions
        guard $ not $ null outputVersions'

        let oAuthor = view _2 $ head outputVersions'
        let oSite = view _3 $ head outputVersions'
        let oLicense = view _4 $ head outputVersions'
        let oVersions = map (view _1) outputVersions'

        oNotifyUploader <- lift $ withProb (1/20) False True
        oNotifyMaintainers <- lift $ withProb (1/10) False True
        oDeprecated <- lift $ withProb (1/80) False True
        oPrivate <- lift $ withProb (1/125) False True

        pure $ OutputPackage{..}

    let justsPackages = catMaybes outputPackages
    putStrLn $ "total packages: " ++ show (length justsPackages)
    normalized <- normalizePackages justsPackages
    print "done"
