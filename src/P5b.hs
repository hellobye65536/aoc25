{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (fromRight)
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

type Interval = (Int, Int)

invalid :: a
invalid = error "invalid"

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight invalid . T.decimal

parseInterval :: T.Text -> Interval
parseInterval tx = case T.split (== '-') tx of
  [l, r] -> (parseDecimal l, parseDecimal r)
  _ -> invalid

intervalLength :: Interval -> Int
intervalLength (l, r) = r - l + 1

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals ((l1, r1) : (l2, r2) : xs)
  | r1 + 1 >= l2 = mergeIntervals $ (l1, max r1 r2) : xs
mergeIntervals (x : xs) = x : mergeIntervals xs
mergeIntervals [] = []

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map intervalLength
      . mergeIntervals
      . sort
      . map parseInterval
      . takeWhile (/= "")
      . T.lines
      . T.strip
