{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tagger.Scoring where

import Data.Int (Int32)

calculateELOWinnerScoreFirst :: Int32 -> Int32 -> (Int32, Int32)
calculateELOWinnerScoreFirst winnerScore loserScore = (round newWinnerScore, round newLoserScore)
  where
    kFactor = 32.0
    winnerFloat = fromIntegral winnerScore
    loserFloat = fromIntegral loserScore
    expectedWinnerScore = 1.0 / (1.0 + 10.0 ** ((loserFloat - winnerFloat) / 400.0))
    expectedLoserScore = 1.0 / (1.0 + 10.0 ** ((winnerFloat - loserFloat) / 400.0))
    newWinnerScore = winnerFloat + kFactor * (1.0 - expectedWinnerScore)
    newLoserScore = loserFloat + kFactor * (0.0 - expectedLoserScore)

