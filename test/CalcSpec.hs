{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec

import Config (FinancingRound(..), OwnedShares(..))
import Calc (worthAtPrice, profitAtPrice)

-- Test data matching config.yaml
testRounds :: [FinancingRound]
testRounds =
  [ FinancingRound
      { name = "seed"
      , sharesIssued = 5
      , issuePrice = 1.23
      , fullyDiluted = 20
      }
  , FinancingRound
      { name = "series A"
      , sharesIssued = 100
      , issuePrice = 3.14
      , fullyDiluted = 200
      }
  ]

testOwned :: [OwnedShares]
testOwned =
  [ OwnedShares { amount = 1, fmv = 1.23 }
  , OwnedShares { amount = 2, fmv = 3.14 }
  , OwnedShares { amount = 2, fmv = 3.14 }
  ]

main :: IO ()
main = hspec $ do
  describe "worthAtPrice" $ do
    describe "good exit (all investors convert)" $ do
      it "returns pro-rata share at $10,000 sale" $ do
        -- At $50/share, both rounds convert
        -- Our 5 shares × $50 = $250
        worthAtPrice testRounds testOwned 10000 `shouldBe` 250

    describe "mixed exit (some convert, some take preference)" $ do
      it "returns pro-rata of remaining at $1,000 sale" $ do
        -- At $5/share, seed converts ($25 > $6.15), Series A converts ($500 > $314)
        -- Our 5 shares × $5 = $25
        worthAtPrice testRounds testOwned 1000 `shouldBe` 25

    describe "bad exit (preferences exceed sale price)" $ do
      it "returns 0 at $100 sale" $ do
        -- Total preferences = $320.15 > $100
        -- Common shareholders get nothing
        worthAtPrice testRounds testOwned 100 `shouldBe` 0

    describe "edge cases" $ do
      it "returns 0 for empty rounds" $ do
        worthAtPrice [] testOwned 10000 `shouldBe` 0

      it "returns 0 for zero sale price" $ do
        worthAtPrice testRounds testOwned 0 `shouldBe` 0

  describe "profitAtPrice" $ do
    describe "good exit (all investors convert)" $ do
      it "subtracts FMV from payout per lot" $ do
        -- At $50/share payout:
        -- Lot 1: (50 - 1.23) × 1 = 48.77
        -- Lot 2: (50 - 3.14) × 2 = 93.72
        -- Lot 3: (50 - 3.14) × 2 = 93.72
        -- Total: 236.21
        profitAtPrice testRounds testOwned 10000 `shouldBe` 236.21

    describe "mixed exit" $ do
      it "subtracts FMV from payout per lot" $ do
        -- At $5/share payout:
        -- Lot 1: (5 - 1.23) × 1 = 3.77
        -- Lot 2: (5 - 3.14) × 2 = 3.72
        -- Lot 3: (5 - 3.14) × 2 = 3.72
        -- Total: 11.21
        profitAtPrice testRounds testOwned 1000 `shouldBe` 11.21

    describe "payout below some FMVs" $ do
      it "only counts lots where payout exceeds FMV" $ do
        -- Create owned shares with mixed FMVs relative to payout
        let mixedOwned =
              [ OwnedShares { amount = 1, fmv = 40 }  -- profit: (50 - 40) × 1 = 10
              , OwnedShares { amount = 2, fmv = 60 }  -- payout < fmv, profit: 0
              ]
        -- At $10,000 sale: payout = $50/share
        profitAtPrice testRounds mixedOwned 10000 `shouldBe` 10

    describe "bad exit" $ do
      it "returns 0 when payout is zero" $ do
        profitAtPrice testRounds testOwned 100 `shouldBe` 0

    describe "edge cases" $ do
      it "returns 0 for empty rounds" $ do
        profitAtPrice [] testOwned 10000 `shouldBe` 0

      it "returns 0 for empty owned shares" $ do
        profitAtPrice testRounds [] 10000 `shouldBe` 0
