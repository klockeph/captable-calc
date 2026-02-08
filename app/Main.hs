module Main (main) where

import Data.Decimal (Decimal, roundTo)
import Options.Applicative

import Calc (worthAtPrice, profitAtPrice)
import Config (Config (..), readConfig)

data Options = Options
  { configPath :: FilePath
  , sellPrice :: Maybe Decimal
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
      ( metavar "CONFIG"
     <> help "Path to YAML config file"
      )
  <*> optional (argument auto
      ( metavar "SELL_PRICE"
     <> help "Company sale price (optional)"
      ))

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Calculate stock option value and profit"
   <> header "captable-calc - a cap table calculator"
    )
  cfg <- readConfig (configPath opts)
  case sellPrice opts of
    Nothing -> print cfg
    Just price -> do
      let rounds = financingRounds cfg
          owned = ownedShares cfg
          worth = roundTo 2 $ worthAtPrice rounds owned price
          profit = roundTo 2 $ profitAtPrice rounds owned price
      putStrLn $ "Worth:  $" ++ show worth
      putStrLn $ "Profit: $" ++ show profit
