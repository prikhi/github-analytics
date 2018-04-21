{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Aeson
import GHC.Generics
import Servant

import qualified Data.Text as T

type API =
    ReqBody '[JSON] AuthData :> Post '[JSON] [RepositoryTraffic]


data AuthData
    = AuthData
        { adUser :: T.Text
        , adPass :: T.Text
        } deriving (Generic, Show)

instance ToJSON AuthData
instance FromJSON AuthData

data RepositoryTraffic
    = RepositoryTraffic
        { rtID :: Integer
        , rtName :: T.Text
        , rtViews :: Integer
        } deriving (Eq, Generic, Show)

instance ToJSON RepositoryTraffic
instance FromJSON RepositoryTraffic
