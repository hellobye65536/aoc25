{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Exception (assert)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (second))
import Data.List (group)
import Data.Text qualified as T
import Data.Text.IO qualified as T

boolInt :: Bool -> Int
boolInt False = 0
boolInt True = 1

makeGrid :: [[Bool]] -> UArray (Int, Int) Bool
makeGrid xs =
  assert (length (group $ map length xs) == 1) $
    A.listArray ((1, 1), (length xs, length $ head xs)) $
      concat xs

step :: UArray (Int, Int) Bool -> UArray (Int, Int) Bool
step grid = A.genArray (A.bounds grid) $ \(r, c) ->
  (grid A.! (r, c))
    && ( sum
           ( map (maybe 0 boolInt . (grid A.!?)) $
               [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], dr /= 0 || dc /= 0]
           )
           < 4
       )

solve :: UArray (Int, Int) Bool -> Int
solve grid = stepCnt + if stepCnt == 0 then 0 else solve grid'
 where
  stepped = step grid
  stepCnt = length $ filter id $ A.elems stepped
  grid' = grid A.// map (second $ const False) (filter snd $ A.assocs stepped)

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . solve
      . makeGrid
      . map (map (== '@') . T.unpack)
      . T.lines
      . T.strip
