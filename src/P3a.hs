{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.List (tails)

solve :: String -> Int
solve xs = maximum $ do
  (x:ys) <- tails xs
  y <- ys
  pure $ read [x,y]

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map (solve . T.unpack)
      . T.lines
      . T.strip
