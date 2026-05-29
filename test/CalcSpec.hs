{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml

import Config (FinancingRound(..), OwnedShares(..))
import Calc (worthAtPrice, profitAtPrice, profitIfExercised, profitAsConfigured, perSharePayout, conversionThresholds, findPriceForPayout, minPriceForUnexercisedProfit, minPriceForConfiguredProfit, minPriceForExercisedBreakeven)

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
  [ OwnedShares { amount = 1, fmv = 1.23, exercised = False }
  , OwnedShares { amount = 2, fmv = 3.14, exercised = False }
  , OwnedShares { amount = 2, fmv = 3.14, exercised = False }
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
              [ OwnedShares { amount = 1, fmv = 40, exercised = False }  -- profit: (50 - 40) × 1 = 10
              , OwnedShares { amount = 2, fmv = 60, exercised = False }  -- payout < fmv, profit: 0
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

  describe "profitIfExercised" $ do
    describe "good exit (all in the money)" $ do
      it "equals profitAtPrice when all lots are profitable" $ do
        -- At $10,000 sale: payout = $50/share, all FMVs < $50
        profitIfExercised testRounds testOwned 10000 `shouldBe` profitAtPrice testRounds testOwned 10000

    describe "bad exit (underwater options)" $ do
      it "returns negative when payout is less than average FMV" $ do
        -- At $100 sale: payout = $0 (preferences exceed sale)
        -- Exercise cost = 1×1.23 + 2×3.14 + 2×3.14 = 13.79
        -- Profit = 0 - 13.79 = -13.79
        profitIfExercised testRounds testOwned 100 `shouldBe` (-13.79)

    describe "partial underwater" $ do
      it "returns correct profit with mixed FMV lots" $ do
        -- Create owned shares where some are underwater
        let mixedOwned =
              [ OwnedShares { amount = 1, fmv = 40, exercised = False }  -- in the money at $50
              , OwnedShares { amount = 2, fmv = 60, exercised = False }  -- underwater at $50
              ]
        -- At $10,000 sale: payout = $50/share
        -- Worth = 3 × $50 = $150
        -- Exercise cost = 1×40 + 2×60 = 160
        -- Profit (exercised) = 150 - 160 = -10
        profitIfExercised testRounds mixedOwned 10000 `shouldBe` (-10)
        -- Compare: profitAtPrice only counts lot 1: (50-40)×1 = 10
        profitAtPrice testRounds mixedOwned 10000 `shouldBe` 10

    describe "edge cases" $ do
      it "returns negative exercise cost for empty rounds" $ do
        -- Worth = 0, exercise cost = 13.79
        profitIfExercised [] testOwned 10000 `shouldBe` (-13.79)

      it "returns 0 for empty owned shares" $ do
        profitIfExercised testRounds [] 10000 `shouldBe` 0

  describe "profitAsConfigured" $ do
    it "equals profitAtPrice when no lots are exercised" $ do
      profitAsConfigured testRounds testOwned 10000 `shouldBe` profitAtPrice testRounds testOwned 10000
      profitAsConfigured testRounds testOwned 1000  `shouldBe` profitAtPrice testRounds testOwned 1000
      profitAsConfigured testRounds testOwned 100   `shouldBe` profitAtPrice testRounds testOwned 100

    it "equals profitIfExercised when all lots are exercised" $ do
      let allExer = map (\o -> o { exercised = True }) testOwned
      profitAsConfigured testRounds allExer 10000 `shouldBe` profitIfExercised testRounds allExer 10000
      profitAsConfigured testRounds allExer 100   `shouldBe` profitIfExercised testRounds allExer 100

    it "mixes per-lot capping based on exercised flag" $ do
      -- At $10,000 sale: payout = $50/share
      -- Lot 1 (fmv 40, exercised): (50-40)*1 = 10
      -- Lot 2 (fmv 60, not exercised): max(0, 50-60)*2 = 0
      -- Lot 3 (fmv 60, exercised): (50-60)*2 = -20
      let mixedOwned =
            [ OwnedShares { amount = 1, fmv = 40, exercised = True }
            , OwnedShares { amount = 2, fmv = 60, exercised = False }
            , OwnedShares { amount = 2, fmv = 60, exercised = True }
            ]
      profitAsConfigured testRounds mixedOwned 10000 `shouldBe` (-10)

  describe "minPriceForConfiguredProfit" $ do
    it "returns Nothing for empty rounds or shares" $ do
      minPriceForConfiguredProfit [] testOwned `shouldBe` Nothing
      minPriceForConfiguredProfit testRounds [] `shouldBe` Nothing

    it "matches minPriceForUnexercisedProfit when no lots are exercised" $ do
      -- With no exercised lots, configured profit is strictly positive at the
      -- same point as the unexercised profit threshold (where payout > min FMV).
      case (minPriceForConfiguredProfit testRounds testOwned, minPriceForUnexercisedProfit testRounds testOwned) of
        (Just cfg, Just unex) -> abs (cfg - unex) `shouldSatisfy` (< 0.05)
        _                     -> expectationFailure "Expected Just values"

    it "finds the break-even when all lots are exercised" $ do
      let allExer = map (\o -> o { exercised = True }) testOwned
      case minPriceForConfiguredProfit testRounds allExer of
        Nothing -> expectationFailure "Expected Just value"
        Just p  -> profitAsConfigured testRounds allExer p `shouldSatisfy` \v -> abs v < 0.1

    it "lands strictly between unex and all-exercised thresholds for a genuinely mixed config" $ do
      -- One exercised lot at FMV 5 (underwater pull) plus two non-exercised lots
      -- at FMVs 2 and 100. profitAsConfigured(P) = (P-5) + max(0,P-2) + max(0,P-100).
      -- For 2 < P < 100: profit = 2P - 7, crosses 0 at P = 3.5.
      -- Both testRounds convert above S=628; payout=3.5 corresponds to S=700.
      let mixedOwned =
            [ OwnedShares { amount = 1, fmv = 5,   exercised = True  }
            , OwnedShares { amount = 1, fmv = 2,   exercised = False }
            , OwnedShares { amount = 1, fmv = 100, exercised = False }
            ]
      case ( minPriceForUnexercisedProfit testRounds mixedOwned
           , minPriceForConfiguredProfit testRounds mixedOwned
           , minPriceForExercisedBreakeven testRounds mixedOwned
           ) of
        (Just unex, Just cfg, Just allEx) -> do
          abs (cfg - 700) `shouldSatisfy` (< 0.05)
          cfg `shouldSatisfy` (> unex)
          cfg `shouldSatisfy` (< allEx)
          -- Configured profit at the returned price should be essentially zero.
          abs (profitAsConfigured testRounds mixedOwned cfg) `shouldSatisfy` (< 0.5)
        _ -> expectationFailure "Expected Just values for all three thresholds"

  describe "conversionThresholds" $ do
    it "returns sorted thresholds based on issue price × fullyDiluted" $ do
      -- Seed: 1.23 × 200 = 246
      -- Series A: 3.14 × 200 = 628
      conversionThresholds testRounds `shouldBe` [246, 628]

    it "returns empty list for empty rounds" $ do
      conversionThresholds [] `shouldBe` []

  describe "findPriceForPayout" $ do
    it "returns Nothing for empty rounds" $ do
      findPriceForPayout [] 10 `shouldBe` Nothing

    it "returns Just 0 for target <= 0" $ do
      findPriceForPayout testRounds 0 `shouldBe` Just 0
      findPriceForPayout testRounds (-5) `shouldBe` Just 0

    it "finds correct price for a target payout" $ do
      -- Verify by checking the payout at the found price
      case findPriceForPayout testRounds 5 of
        Nothing -> expectationFailure "Expected Just value"
        Just price -> perSharePayout testRounds price `shouldBe` 5

    it "finds correct price for higher target payout" $ do
      case findPriceForPayout testRounds 50 of
        Nothing -> expectationFailure "Expected Just value"
        Just price -> perSharePayout testRounds price `shouldBe` 50

  describe "minPriceForUnexercisedProfit" $ do
    it "returns Nothing for empty rounds" $ do
      minPriceForUnexercisedProfit [] testOwned `shouldBe` Nothing

    it "returns Nothing for empty owned shares" $ do
      minPriceForUnexercisedProfit testRounds [] `shouldBe` Nothing

    it "finds price where payout equals minimum FMV" $ do
      -- Minimum FMV in testOwned is 1.23
      case minPriceForUnexercisedProfit testRounds testOwned of
        Nothing -> expectationFailure "Expected Just value"
        Just price -> perSharePayout testRounds price `shouldBe` 1.23

  describe "minPriceForExercisedBreakeven" $ do
    it "returns Nothing for empty rounds" $ do
      minPriceForExercisedBreakeven [] testOwned `shouldBe` Nothing

    it "returns Nothing for empty owned shares" $ do
      minPriceForExercisedBreakeven testRounds [] `shouldBe` Nothing

    it "finds price where profitIfExercised first reaches 0 (payout = avg FMV)" $ do
      -- Total exercise cost = 1×1.23 + 2×3.14 + 2×3.14 = 13.79
      -- Total shares = 5; weighted-avg FMV = 13.79 / 5 = 2.758
      case minPriceForExercisedBreakeven testRounds testOwned of
        Nothing -> expectationFailure "Expected Just value"
        Just price -> do
          perSharePayout testRounds price `shouldBe` 2.758
          -- At break-even, profitIfExercised should be 0
          profitIfExercised testRounds testOwned price `shouldBe` 0

  describe "OwnedShares YAML parsing" $ do
    it "defaults exercised to False when the field is omitted" $ do
      let yaml = BS.pack "amount: 5\nfmv: 1.5\n"
      case Yaml.decodeThrow yaml :: Maybe OwnedShares of
        Just o  -> exercised o `shouldBe` False
        Nothing -> expectationFailure "Expected successful parse"

    it "respects exercised: true" $ do
      let yaml = BS.pack "amount: 5\nfmv: 1.5\nexercised: true\n"
      case Yaml.decodeThrow yaml :: Maybe OwnedShares of
        Just o  -> exercised o `shouldBe` True
        Nothing -> expectationFailure "Expected successful parse"

    it "respects exercised: false" $ do
      let yaml = BS.pack "amount: 5\nfmv: 1.5\nexercised: false\n"
      case Yaml.decodeThrow yaml :: Maybe OwnedShares of
        Just o  -> exercised o `shouldBe` False
        Nothing -> expectationFailure "Expected successful parse"
