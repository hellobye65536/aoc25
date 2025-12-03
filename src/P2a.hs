{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (join, replicateM)
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

type Interval = (Int, Int)

invalid :: a
invalid = error "invalid"

parseInterval :: T.Text -> Interval
parseInterval tx = case T.split (== '-') tx of
  [l, r] -> (parseDecimal l, parseDecimal r)
  _ -> invalid
 where
  parseDecimal = fst . fromRight invalid . T.decimal

inInterval :: Int -> Interval -> Bool
inInterval x (l, r) = l <= x && x <= r

makeCandidates :: Int -> [Int]
makeCandidates maxDigits = do
  digits <- [1 .. (maxDigits + 1) `quot` 2]
  first <- [1 .. 9] :: [Int]
  rest <- replicateM (digits - 1) [0 .. 9] :: [[Int]]
  pure $ read $ join (<>) $ mconcat $ map show $ first : rest

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . ( \intervals ->
            filter
              (\c -> any (inInterval c) intervals)
              $ makeCandidates
              $ maximum
              $ map (length . show . snd) intervals
        )
      . map parseInterval
      . T.split (== ',')
      . T.strip
