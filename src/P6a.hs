{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (transpose, unsnoc)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

invalid :: a
invalid = error "invalid"

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight invalid . T.decimal

parseOp :: Char -> [Int] -> Int
parseOp '+' = sum
parseOp '*' = product
parseOp _ = invalid

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . uncurry (zipWith (&))
      . bimap
        (transpose . map (map parseDecimal))
        (map (parseOp . T.head))
      . fromMaybe invalid
      . unsnoc
      . map T.words
      . T.lines
      . T.strip
