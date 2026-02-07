module Calc (worthAtPrice, profitAtPrice, perSharePayout) where

import Data.Decimal (Decimal)
import Data.List (partition)

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

-- | Determine if a round takes preference or converts.
-- Investors convert when conversion value exceeds preference value.
takesPreference :: Decimal -> FinancingRound -> Bool
takesPreference pricePerShare r = issuePrice r >= pricePerShare
