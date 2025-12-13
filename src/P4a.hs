{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Exception (assert)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.List (group)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse :: Char -> Int
parse '@' = 1
parse _ = 0

makeGrid :: [[Int]] -> UArray (Int, Int) Int
makeGrid xs =
  assert (length (group $ map length xs) == 1) $
    A.listArray ((1, 1), (length xs, length $ head xs)) $
      concat xs

solve :: UArray (Int, Int) Int -> UArray (Int, Int) Bool
solve grid = A.genArray (A.bounds grid) $ \(r, c) ->
  (grid A.! (r, c) == 1)
    && ( sum
           ( map (fromMaybe 0 . (grid A.!?)) $
               [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], dr /= 0 || dc /= 0]
           )
           < 4
       )

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . length
      . filter id
      . A.elems
      . solve
      . makeGrid
      . map (map parse . T.unpack)
      . T.lines
      . T.strip
