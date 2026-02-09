module Calc
  ( worthAtPrice
  , profitAtPrice
  , profitIfExercised
  , perSharePayout
  , conversionThresholds
  , findPriceForPayout
  , minPriceForUnexercisedProfit
  , minPriceForExercisedBreakeven
  ) where

import Data.Decimal (Decimal)
import Data.List (partition, sort)

import Config (FinancingRound (..), OwnedShares (..))

-- | Calculate total shares we own.
totalOwnedShares :: [OwnedShares] -> Int
totalOwnedShares = sum . fmap amount

-- | Calculate common shares (fully diluted minus investor shares).
commonShareCount :: [FinancingRound] -> Integer
commonShareCount rounds = fullyDiluted (last rounds) - round (sum $ sharesIssued <$> rounds)

-- | Sum of preference values for given rounds.
totalPreferenceAmount :: [FinancingRound] -> Decimal
totalPreferenceAmount = sum . fmap preferenceValue
  where
    preferenceValue r = sharesIssued r * issuePrice r

-- | Calculate shares participating in remaining proceeds.
-- This includes converting investor shares plus common shares.
participatingShareCount :: [FinancingRound] -> [FinancingRound] -> Decimal
participatingShareCount allRounds convertingRounds =
  sum (sharesIssued <$> convertingRounds) + fromIntegral (commonShareCount allRounds)

-- | Calculate payout per participating share.
payoutPerShare :: Decimal -> Decimal -> Decimal
payoutPerShare remaining participating
  | participating > 0 = remaining / participating
  | otherwise         = 0

-- | Calculate per-share payout at a given sale price.
perSharePayout :: [FinancingRound] -> Decimal -> Decimal
perSharePayout [] _ = 0
perSharePayout rounds salePrice
  | totalPref >= salePrice = 0
  | otherwise              = payoutPerShare remaining participating
  where
    pricePerShare = salePrice / fromIntegral (fullyDiluted $ last rounds)
    (prefRounds, convertRounds) = partition (takesPreference pricePerShare) rounds
    totalPref     = totalPreferenceAmount prefRounds
    remaining     = max 0 (salePrice - totalPref)
    participating = participatingShareCount rounds convertRounds

-- | Calculate what owned shares are worth at a given company sale price.
-- Uses 1x non-participating preferred liquidation preferences.
worthAtPrice :: [FinancingRound] -> [OwnedShares] -> Decimal -> Decimal
worthAtPrice rounds owned salePrice =
  fromIntegral (totalOwnedShares owned) * perSharePayout rounds salePrice

-- | Calculate profit from stock options at a given sale price.
-- For each lot, profit = (perSharePayout - fmv) * amount, but only if positive.
profitAtPrice :: [FinancingRound] -> [OwnedShares] -> Decimal -> Decimal
profitAtPrice rounds owned salePrice = sum $ lotProfit <$> owned
  where
    payout = perSharePayout rounds salePrice
    lotProfit lot
      | payout > fmv lot = (payout - fmv lot) * fromIntegral (amount lot)
      | otherwise        = 0

-- | Calculate profit if all options were already exercised.
-- Unlike profitAtPrice, this can be negative (underwater options).
profitIfExercised :: [FinancingRound] -> [OwnedShares] -> Decimal -> Decimal
profitIfExercised rounds owned salePrice = worth - exerciseCost
  where
    worth = worthAtPrice rounds owned salePrice
    exerciseCost = sum $ (\lot -> fmv lot * fromIntegral (amount lot)) <$> owned

-- | Determine if a round takes preference or converts.
-- Investors convert when conversion value exceeds preference value.
takesPreference :: Decimal -> FinancingRound -> Bool
takesPreference pricePerShare r = issuePrice r >= pricePerShare

-- | Get sorted conversion thresholds for all rounds.
-- Each threshold is a sale price where a round switches from preference to conversion.
-- A round converts when salePrice / fullyDiluted > issuePrice,
-- i.e., when salePrice > issuePrice * fullyDiluted.
conversionThresholds :: [FinancingRound] -> [Decimal]
conversionThresholds rounds = sort $ threshold <$> rounds
  where
    threshold r = issuePrice r * fromIntegral (fullyDiluted $ last rounds)

-- | Find minimum sale price achieving target per-share payout.
-- Iterates through intervals defined by conversion thresholds,
-- solving linear equations in each interval.
findPriceForPayout :: [FinancingRound] -> Decimal -> Maybe Decimal
findPriceForPayout [] _ = Nothing
findPriceForPayout _ target | target <= 0 = Just 0
findPriceForPayout rounds target = findInIntervals intervals
  where
    thresholds = conversionThresholds rounds
    -- Create intervals: [(0, t1), (t1, t2), ..., (tn, Nothing)]
    -- Nothing represents unbounded upper end
    intervals = zip (0 : thresholds) (fmap Just thresholds ++ [Nothing])

    findInIntervals :: [(Decimal, Maybe Decimal)] -> Maybe Decimal
    findInIntervals ((lo, mHi):rest)
      | isValidInInterval = Just candidate
      | otherwise = findInIntervals rest
      where
        -- Calculate at a point inside the interval to determine which rounds convert
        testPoint = case mHi of
          Just hi -> (lo + hi) / 2
          Nothing -> lo + 1
        pricePerShare = testPoint / fromIntegral (fullyDiluted $ last rounds)
        (prefRounds, convertRounds) = partition (takesPreference pricePerShare) rounds
        totalPref = totalPreferenceAmount prefRounds
        participating = participatingShareCount rounds convertRounds

        -- In this interval: payout = (salePrice - totalPref) / participating
        -- Solve: target = (salePrice - totalPref) / participating
        -- => salePrice = target * participating + totalPref
        candidate = target * participating + totalPref

        isValidInInterval =
          participating > 0 &&
          candidate > lo &&
          case mHi of
            Just hi -> candidate <= hi
            Nothing -> True

    findInIntervals [] = Nothing

-- | Minimum sale price where payout equals smallest FMV (unexercised options profit).
minPriceForUnexercisedProfit :: [FinancingRound] -> [OwnedShares] -> Maybe Decimal
minPriceForUnexercisedProfit [] _ = Nothing
minPriceForUnexercisedProfit _ [] = Nothing
minPriceForUnexercisedProfit rounds owned =
  findPriceForPayout rounds (minimum $ fmv <$> owned)

-- | Minimum sale price where profit equals total exercise cost (break-even after exercising all).
-- This finds the price where: sum((payout - fmv_i) * amount_i) = sum(fmv_i * amount_i)
-- which simplifies to: sum(payout * amount_i) = 2 * sum(fmv_i * amount_i)
-- => payout * totalShares = 2 * totalExerciseCost
-- => targetPayout = 2 * totalExerciseCost / totalShares
minPriceForExercisedBreakeven :: [FinancingRound] -> [OwnedShares] -> Maybe Decimal
minPriceForExercisedBreakeven [] _ = Nothing
minPriceForExercisedBreakeven _ [] = Nothing
minPriceForExercisedBreakeven rounds owned
  | totalShares == 0 = Nothing
  | otherwise = findPriceForPayout rounds targetPayout
  where
    totalShares = fromIntegral $ sum (amount <$> owned)
    totalExerciseCost = sum $ (\lot -> fmv lot * fromIntegral (amount lot)) <$> owned
    -- For break-even: profit = exercise cost
    -- profit = sum((payout - fmv_i) * amount_i) for lots where payout > fmv_i
    -- At break-even with all lots profitable: payout * total - totalCost = totalCost
    -- => payout = 2 * totalCost / total
    targetPayout = 2 * totalExerciseCost / totalShares
