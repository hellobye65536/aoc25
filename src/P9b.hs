{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Monad (forM_, guard, join, unless)
import Data.Array.MArray qualified as AM
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed qualified as AU
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Ix (inRange)
import Data.List (sort, tails)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Unboxed qualified as VU

type Vec2 = (Int, Int)

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight undefined . T.decimal

parseVec2 :: T.Text -> Vec2
parseVec2 tx = case parseDecimal <$> T.split (== ',') tx of
  [x, y] -> (x, y)
  _ -> undefined

rect :: Vec2 -> Vec2 -> Int
rect (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

edge :: Vec2 -> Vec2 -> [Vec2]
edge (x1, y1) (x2, y2)
  | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2]
  | y1 == y2 = map (,y1) [min x1 x2 .. max x1 x2]
  | otherwise = undefined

buildOuter :: [Vec2] -> AU.UArray Vec2 Bool
buildOuter pts = runSTUArray $ do
  let x1 = minimum $ map fst pts
      y1 = minimum $ map snd pts
      x2 = maximum $ map fst pts
      y2 = maximum $ map snd pts
      bounds = ((x1, y1), (x2, y2))
      ptsEdges = concatMap (uncurry edge) $ zip pts $ tail pts
      gridEdges =
        concat
          [ edge (x1, y1) (x2, y1)
          , edge (x1, y1) (x1, y2)
          , edge (x2, y1) (x2, y2)
          , edge (x1, y2) (x2, y2)
          ]
  outer <- AM.newArray bounds False
  forM_ ptsEdges $ \i -> AM.writeArray outer i True
  let fill (x, y) = do
        filled <-
          if inRange bounds (x, y)
            then AM.readArray outer (x, y)
            else pure True
        unless filled $ do
          AM.writeArray outer (x, y) True
          fill (x + 1, y)
          fill (x - 1, y)
          fill (x, y + 1)
          fill (x, y - 1)
  forM_ gridEdges fill
  forM_ ptsEdges $ \i -> AM.writeArray outer i False
  pure outer

buildPSA :: AU.UArray Vec2 Bool -> AU.UArray Vec2 Int
buildPSA outer = runSTUArray $ do
  let ((x1, y1), (x2, y2)) = AU.bounds outer
  psa <- AM.newArray ((x1 - 1, y1 - 1), (x2, y2)) 0
  forM_ (AU.indices outer) $ \(x, y) -> do
    AM.writeArray psa (x, y) . sum
      =<< sequenceA
        [ AM.readArray psa (x - 1, y)
        , AM.readArray psa (x, y - 1)
        , negate <$> AM.readArray psa (x - 1, y - 1)
        , pure $ if outer AU.! (x, y) then 0 else 1
        ]
  pure psa

minmax :: Ord a => a -> a -> (a, a)
minmax a b = (min a b, max a b)

rectPSA :: AU.UArray Vec2 Int -> Vec2 -> Vec2 -> Int
rectPSA psa (x1, y1) (x2, y2) =
  psa AU.! (x2', y2')
    - psa AU.! (x1' - 1, y2')
    - psa AU.! (x2', y1' - 1)
    + psa AU.! (x1' - 1, y1' - 1)
 where
  (x1', x2') = minmax x1 x2
  (y1', y2') = minmax y1 y2

solve :: [Vec2] -> Int
solve pts = maximum $ do
  (u : vs) <- tails compressedPts
  v <- vs
  let area = rect u v
      psaArea = rectPSA psa u v
  guard $ area == psaArea
  pure $ rect (decompress u) (decompress v)
 where
  compressTables = join bimap (VU.uniq . VU.fromList . sort) $ unzip pts
  (xCmp, yCmp) = compressTables
  compress (x, y) =
    let x' = fromMaybe undefined $ VU.findIndex (== x) xCmp
        y' = fromMaybe undefined $ VU.findIndex (== y) yCmp
     in (x', y')
  decompress (x, y) =
    let x' = xCmp VU.! x
        y' = yCmp VU.! y
     in (x', y')
  compressedPts = map compress pts
  outer = buildOuter (last compressedPts : compressedPts)
  psa = buildPSA outer

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . solve
      . map parseVec2
      . T.lines
      . T.strip
