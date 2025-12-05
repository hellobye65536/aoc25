{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Bifunctor (bimap)

type Interval = (Int, Int)

invalid :: a
invalid = error "invalid"

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight invalid . T.decimal

parseInterval :: T.Text -> Interval
parseInterval tx = case T.split (== '-') tx of
  [l, r] -> (parseDecimal l, parseDecimal r)
  _ -> invalid

inInterval :: Int -> Interval -> Bool
inInterval x (l, r) = l <= x && x <= r

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . length
      . (\(ints, xs) -> filter (\x -> any (inInterval x) ints) xs)
      . bimap (map parseInterval) (map parseDecimal . drop 1)
      . break (== "")
      . T.lines
      . T.strip
