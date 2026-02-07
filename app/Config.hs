{-# LANGUAGE DeriveGeneric #-}

module Config where

import GHC.Generics (Generic)
import Data.Yaml (decodeFileThrow, FromJSON)
import Data.Text (Text)

data FinancingRound = FinancingRound
  { name :: Text
  , sharesIssued :: Double
  , issuePrice :: Double
  , fullyDiluted :: Double
  } deriving (Show, Generic)

instance FromJSON FinancingRound


data OwnedShares = OwnedShares
  {
    amount :: Int
  , roundIssued :: Int
  } deriving (Show, Generic)

instance FromJSON OwnedShares

data Config = Config
  {
    financingRounds :: [FinancingRound]
  , ownedShares :: [OwnedShares]
  } deriving (Show, Generic)

instance FromJSON Config

readConfig :: IO Config
readConfig = decodeFileThrow "config.yaml" :: IO Config
