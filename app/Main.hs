module Main (main) where

import Data.Char (toLower)
import Data.Decimal (Decimal, roundTo)
import Numeric (showFFloat)
import Options.Applicative

import Calc (worthAtPrice, profitAtPrice, profitIfExercised, minPriceForUnexercisedProfit, minPriceForExercisedBreakeven)
import Config (Config (..), readConfig)

readPrice :: ReadM Decimal
readPrice = eitherReader $ \s ->
  let (numStr, suffix) = break (`elem` "kKmMbBtT") s
      parseNum n = case reads n :: [(Decimal, String)] of
        [(d, "")] -> Right d
        _         -> Left ("Invalid number: " ++ n)
      multiplier "" = Right 1
      multiplier [c] = case toLower c of
        'k' -> Right 1e3
        'm' -> Right 1e6
        'b' -> Right 1e9
        't' -> Right 1e12
        _   -> Left ("Unknown suffix: " ++ [c])
      multiplier cs = Left ("Unknown suffix: " ++ cs)
  in (*) <$> parseNum numStr <*> multiplier suffix

formatDollars :: Decimal -> String
formatDollars d =
  let base = "$" ++ show (roundTo 2 d)
      n = realToFrac d :: Double
      absN = abs n
      fmt x = showFFloat (Just 2) x ""
      suffix
        | absN >= 1e12 = Just (fmt (n / 1e12) ++ "T")
        | absN >= 1e9  = Just (fmt (n / 1e9)  ++ "B")
        | absN >= 1e6  = Just (fmt (n / 1e6)  ++ "M")
        | absN >= 1e3  = Just (fmt (n / 1e3)  ++ "K")
        | otherwise    = Nothing
  in case suffix of
       Nothing -> base
       Just s  -> base ++ " (~" ++ s ++ ")"

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
  <*> optional (argument readPrice
      ( metavar "SELL_PRICE"
     <> help "Company sale price, e.g. 4000000000, 4B, 1.5B (optional)"
      ))

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Calculate stock option value and profit"
   <> header "captable-calc - a cap table calculator"
    )
  cfg <- readConfig (configPath opts)
  let rounds = financingRounds cfg
      owned = ownedShares cfg
  case sellPrice opts of
    Nothing -> do
      putStrLn "Inflection Points:"
      case minPriceForUnexercisedProfit rounds owned of
        Nothing -> putStrLn "  Unexercised profit threshold: N/A"
        Just p -> putStrLn $ "  Unexercised profit threshold: " ++ formatDollars p
      case minPriceForExercisedBreakeven rounds owned of
        Nothing -> putStrLn "  Exercised break-even threshold: N/A"
        Just p -> putStrLn $ "  Exercised break-even threshold: " ++ formatDollars p
    Just price -> do
      putStrLn $ "Worth:  " ++ formatDollars (worthAtPrice rounds owned price)
      putStrLn $ "Profit (unexercised): " ++ formatDollars (profitAtPrice rounds owned price)
      putStrLn $ "Profit (exercised):   " ++ formatDollars (profitIfExercised rounds owned price)
