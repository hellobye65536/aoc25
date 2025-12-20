{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Monad (guard)
import Control.Monad.ST (runST)
import Data.Either (fromRight)
import Data.HashTable.ST.Linear qualified as HT
import Data.Maybe (fromMaybe)
import Data.Semigroup (Min (Min, getMin))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Unboxed qualified as V

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight (error "bad decimal") . T.decimal

debracket :: T.Text -> T.Text
debracket = T.dropEnd 1 . T.drop 1

parseDecimalList :: T.Text -> [Int]
parseDecimalList = map parseDecimal . T.split (== ',')

parse :: T.Text -> ([[Int]], V.Vector Int)
parse tx = (buttons, counters)
 where
  broken = map debracket $ T.split (== ' ') tx
  buttons = map parseDecimalList $ tail $ init broken
  counters = V.fromList $ parseDecimalList $ last broken

decrementCounters :: V.Vector Int -> [Int] -> V.Vector Int
decrementCounters counters idxs =
  V.accum
    (const . subtract 1)
    counters
    $ map (,()) idxs

backtrackPressed :: ([[Int]], V.Vector Int) -> [[[Int]]]
backtrackPressed (_, counters) | V.any (< 0) counters = []
backtrackPressed ([], _) = [[]]
backtrackPressed (btn : buttons, counters) =
  map (btn :) (backtrackPressed (buttons, counters')) <> backtrackPressed (buttons, counters)
 where
  counters' = decrementCounters counters btn

solve :: ([[Int]], V.Vector Int) -> Maybe Int
solve (buttons, counters_) = runST $ do
  memo <- HT.new
  let go counters | V.all (== 0) counters = pure $ Just 0
      go counters =
        HT.lookup memo memoKey >>= \case
          Just res -> pure res
          Nothing -> do
            computed <- computed_
            HT.insert memo memoKey computed
            pure computed
       where
        memoKey = V.toList counters
        computed_ = fmap (fmap getMin . foldMap (fmap Min)) $ sequenceA $ do
          pressed <- backtrackPressed (buttons, counters)

          let counters' = decrementCounters counters (concat pressed)
          guard $ V.all even counters'

          let halvedCounters = V.map (`quot` 2) counters'
          pure $ fmap (\nextPressed -> length pressed + 2 * nextPressed) <$> go halvedCounters
  go counters_

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map (fromMaybe (error "unsolveable") . solve . parse)
      . T.lines
      . T.strip
