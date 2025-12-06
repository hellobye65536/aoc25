{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (transpose, unsnoc)
import Data.List.Split (splitWhen)
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
      . map
        ( uncurry (&)
            . bimap (map (parseDecimal . T.strip) . T.transpose) (parseOp . T.head . T.strip)
            . fromMaybe invalid
            . unsnoc
            . map T.pack
            . transpose
        )
      . splitWhen (all (== ' '))
      . transpose
      . map T.unpack
      . T.lines
      . T.strip
