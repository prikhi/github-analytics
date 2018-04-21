{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent.Async (forConcurrently)
import Control.Lens ((^.), (?~), (&), to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:), FromJSON(..), withObject, decode)
import Data.Proxy (Proxy(..))
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCorsResourcePolicy, cors, corsRequestHeaders)
import Network.Wreq (defaults, auth, basicAuth, getWith, responseBody)

import Common

import qualified Data.Text as T


main :: IO ()
main =
    run 9021 $ cors (const $ Just policy) app
    where
        policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"] }


app :: Application
app =
    serve (Proxy :: Proxy API) server

server :: Server API
server = getTraffic

getTraffic :: AuthData -> Handler [RepositoryTraffic]
getTraffic aData =
    getRepositories aData >>= \case
        Nothing ->
            return []
        Just repos ->
            fmap catMaybes . liftIO . forConcurrently (filter (not . ghIsFork) repos) $ \repo ->
                getViews aData repo >>= \case
                    Nothing ->
                        return Nothing
                    Just views  ->
                        return
                            $ Just
                            $ RepositoryTraffic (ghID repo) (ghName repo) (ghCount views)


getRepositories :: MonadIO m => AuthData -> m (Maybe [GHRepo])
getRepositories aData =
    apiRequest aData
        $ baseUrl <> "/user/repos?affiliation=owner&per_page=100"

getViews :: MonadIO m => AuthData -> GHRepo -> m (Maybe GHViews)
getViews aData (GHRepo _ n _) =
    apiRequest aData
        $ baseUrl <> "/repos/" <> T.unpack (adUser aData) <> "/" <> T.unpack n <> "/traffic/views"

baseUrl :: String
baseUrl = "https://api.github.com"


apiRequest :: (FromJSON a, MonadIO m) => AuthData -> String -> m (Maybe a)
apiRequest (AuthData user pass) url =
    let
        config =
            defaults & auth ?~ basicAuth (encodeUtf8 user) (encodeUtf8 pass)
    in do
        resp <- liftIO $ getWith config url
        return $ resp ^. responseBody . to decode


-- GET https://api.github.com/user/repos

data GHRepo
    = GHRepo
    { ghID :: Integer
    , ghName :: T.Text
    , ghIsFork :: Bool
    }

instance FromJSON GHRepo where
    parseJSON = withObject "GHRepo" $ \v ->
        GHRepo
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "fork"

data GHViews
    = GHViews
        { ghCount :: Integer
        , ghUniques :: Integer
        }

instance FromJSON GHViews where
    parseJSON = withObject "GHViews" $ \v ->
        GHViews
            <$> v .: "count"
            <*> v .: "uniques"
