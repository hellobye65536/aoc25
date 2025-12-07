{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function (on)
import Data.List (singleton, sort, uncons)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxed qualified as V

invalid :: a
invalid = error "invalid"

step :: [(Int, Int)] -> V.Vector Bool -> [(Int, Int)]
step idxs row =
  map
    (\g -> (fst $ NE.head g, sum $ map snd $ NE.toList g))
    $ NE.groupBy ((==) `on` fst)
    $ sort
    $ keeps <> concatMap (\(x, cnt) -> map (,cnt) [x - 1, x + 1]) splits
 where
  splits = filter (or . (row V.!?) . fst) idxs
  keeps = filter (all not . (row V.!?) . fst) idxs

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map snd
      . uncurry
        ( foldl step
            . singleton
            . (,1)
            . fromMaybe invalid
            . V.findIndex id
        )
      . fromMaybe invalid
      . uncons
      . map (V.fromList . map (/= '.') . T.unpack)
      . T.lines
      . T.strip
