{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (fromRight)
import Data.List (tails)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

type Vec2 = (Int, Int)

invalid :: a
invalid = error "invalid"

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight invalid . T.decimal

parseVec2 :: T.Text -> Vec2
parseVec2 tx = case parseDecimal <$> T.split (== ',') tx of
  [x, y] -> (x, y)
  _ -> invalid

rect :: Vec2 -> Vec2 -> Int
rect (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . maximum
      . map (uncurry rect)
      . concatMap
        ( \case
            (x : xs) -> (x,) <$> xs
            _ -> []
        )
      . tails
      . map parseVec2
      . T.lines
      . T.strip
