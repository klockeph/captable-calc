module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Decimal (Decimal, roundTo)
import Data.List (intercalate, transpose)
import Numeric (showFFloat)
import Options.Applicative

import Calc
  ( worthAtPrice, profitAtPrice, profitIfExercised, profitAsConfigured
  , minPriceForUnexercisedProfit, minPriceForConfiguredProfit, minPriceForExercisedBreakeven
  )
import Config (Config (..), OwnedShares (..), readConfig)

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

humanSuffix :: Decimal -> Maybe String
humanSuffix d =
  let n = realToFrac d :: Double
      absN = abs n
      fmt x = showFFloat (Just 2) x ""
  in if      absN >= 1e12 then Just (fmt (n / 1e12) ++ "T")
     else if absN >= 1e9  then Just (fmt (n / 1e9)  ++ "B")
     else if absN >= 1e6  then Just (fmt (n / 1e6)  ++ "M")
     else if absN >= 1e3  then Just (fmt (n / 1e3)  ++ "K")
     else                      Nothing

formatDollars :: Decimal -> String
formatDollars d =
  let base = "$" ++ show (roundTo 2 d)
  in case humanSuffix d of
       Nothing -> base
       Just s  -> base ++ " (~" ++ s ++ ")"

humanDollars :: Decimal -> String
humanDollars d = case humanSuffix d of
  Just s  -> "$" ++ s
  Nothing -> "$" ++ show (roundTo 2 d)

renderTable :: [[String]] -> String
renderTable rows =
  let widths = map (maximum . map length) (transpose rows)
      padRow = zipWith (\w s -> s ++ replicate (w - length s) ' ') widths
  in intercalate "\n" (map (intercalate "  " . padRow) rows)

data Options = Options
  { configPath :: FilePath
  , extensive  :: Bool
  , sellPrices :: [Decimal]
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
      ( metavar "CONFIG"
     <> help "Path to YAML config file"
      )
  <*> switch
      ( long "extensive"
     <> short 'e'
     <> help "Show three Profit columns (unexercised / as config / all exercised) instead of one"
      )
  <*> many (argument readPrice
      ( metavar "SELL_PRICE..."
     <> help "Company sale price(s), e.g. 4000000000, 4B, 1.5B. Pass multiple for a sensitivity table. Profit can be negative when exercised lots are underwater."
      ))

formatThreshold :: String -> Maybe Decimal -> String
formatThreshold label mp = "  " ++ label ++ ": " ++ case mp of
  Nothing -> "N/A"
  Just p  -> formatDollars p

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
      strikePaid = sum [ fmv lot * fromIntegral (amount lot) | lot <- owned, exercised lot ]
  when (strikePaid > 0) $ do
    putStrLn $ "Strike paid (exercised lots): " ++ humanDollars strikePaid
    putStrLn ""
  case sellPrices opts of
    [] -> do
      putStrLn "Inflection Points:"
      putStrLn $ formatThreshold "Not exercised"      (minPriceForUnexercisedProfit rounds owned)
      putStrLn $ formatThreshold "Exercised as config" (minPriceForConfiguredProfit rounds owned)
      putStrLn $ formatThreshold "All exercised"      (minPriceForExercisedBreakeven rounds owned)
    prices -> do
      let headerRow
            | extensive opts = ["Exit Price", "Proceeds", "Profit (unex.)", "Profit (as cfg)", "Profit (exer.)"]
            | otherwise      = ["Exit Price", "Proceeds", "Profit"]
          row p
            | extensive opts =
                [ humanDollars p
                , humanDollars (worthAtPrice rounds owned p)
                , humanDollars (profitAtPrice rounds owned p)
                , humanDollars (profitAsConfigured rounds owned p)
                , humanDollars (profitIfExercised rounds owned p)
                ]
            | otherwise =
                [ humanDollars p
                , humanDollars (worthAtPrice rounds owned p)
                , humanDollars (profitAsConfigured rounds owned p)
                ]
      putStrLn $ renderTable (headerRow : map row prices)
