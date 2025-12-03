{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List ((!?))
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as T

maxCount :: Int
maxCount = 12

solve :: String -> Maybe Int
solve xs = go xs !! (maxCount - 1)
 where
  go :: String -> [Maybe Int]
  go [] = replicate maxCount Nothing
  go (x : xs) = flip map [0 .. maxCount - 1] $ \i ->
    max
      (prev !! i)
      (read . (x :) <$> maybe (Just "") (fmap show) (prev !? (i - 1)))
   where
    prev = go xs

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map (fromJust . solve . T.unpack)
      . T.lines
      . T.strip
