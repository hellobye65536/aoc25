{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (singleton, sort, uncons)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxed qualified as V

invalid :: a
invalid = error "invalid"

step :: ([Int], Int) -> V.Vector Bool -> ([Int], Int)
step (idxs, _) row =
  ( map
      NE.head
      $ NE.group
      $ sort
      $ keeps <> concatMap (\x -> [x - 1, x + 1]) splits
  , length splits
  )
 where
  splits = filter (or . (row V.!?)) idxs
  keeps = filter (all not . (row V.!?)) idxs

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map snd
      . uncurry (scanl step . (,0) . singleton . fromMaybe invalid . V.findIndex id)
      . fromMaybe invalid
      . uncons
      . map (V.fromList . map (/= '.') . T.unpack)
      . T.lines
      . T.strip
