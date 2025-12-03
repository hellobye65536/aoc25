{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Either (fromRight)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
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
  let res = mconcat $ map show $ first : rest
  dups <- [2 .. quot maxDigits digits]
  pure $ read $ mconcat $ replicate dups res

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . ( \intervals ->
            map NE.head
              $ NE.group
              $ sort
              $ filter
                (\c -> any (c `inInterval`) intervals)
              $ makeCandidates
              $ maximum
              $ map (length . show . snd) intervals
        )
      . map parseInterval
      . T.split (== ',')
      . T.strip
