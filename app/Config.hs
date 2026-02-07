{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Config where

import GHC.Generics (Generic)
import Data.Yaml (decodeFileThrow, FromJSON(..))
import Data.Aeson (withScientific)
import Data.Text (Text)
import Data.Decimal (Decimal, realFracToDecimal)

instance FromJSON Decimal where
  parseJSON = withScientific "Decimal" $ \s ->
    pure $ realFracToDecimal 10 s

data FinancingRound = FinancingRound
  { name :: Text
  , sharesIssued :: Decimal
  , issuePrice :: Decimal
  , fullyDiluted :: Integer
  } deriving (Show, Generic)

instance FromJSON FinancingRound


data OwnedShares = OwnedShares
  { amount :: Int
  , fmv :: Decimal  -- Fair Market Value per share at time of acquisition
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
