{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (join, when)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as VM

type Vec3 = (Int, Int, Int)

invalid :: a
invalid = error "invalid"

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight invalid . T.decimal

parseVec3 :: T.Text -> Vec3
parseVec3 tx = case parseDecimal <$> T.split (== ',') tx of
  [x, y, z] -> (x, y, z)
  _ -> invalid

norm :: Vec3 -> Int
norm (x, y, z) = x * x + y * y + z * z

diff :: Vec3 -> Vec3 -> Vec3
diff (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

dist :: Vec3 -> Vec3 -> Int
dist = (norm .) . diff

{- HLINT ignore solve' "Use infix" -}
solve' :: [Vec3] -> (V.Vector Int, V.Vector Int)
solve' vecs_ = V.unzip $ V.create $ do
  let vecs = V.fromList vecs_
      vl = V.length vecs
  parents <- VM.generate vl id
  size <- VM.generate vl (const (1 :: Int))
  let
    find x = do
      px <- VM.read parents x
      if x == px
        then pure px
        else do
          found <- find px
          VM.write parents x found
          pure found
    union x y = do
      x' <- find x
      y' <- find y
      if x' == y'
        then pure False
        else do
          (x'', y'') <- do
            sx <- VM.read size x'
            sy <- VM.read size y'
            pure $ if sx < sy then (y', x') else (x', y')
          VM.write parents y'' x''
          sy <- VM.read size y''
          VM.modify size (+ sy) x''
          pure True
    go [] _ = invalid
    go ((u, v) : edges) left = when (left > (0 :: Int)) $ do
      _ <- union u v
      go edges (left - 1)
    edges =
      sortOn
        (uncurry dist . join bimap (vecs V.!))
        [(u, v) | u <- [0 .. vl - 1], v <- [u + 1 .. vl - 1]]
  go edges 1000
  pure $ VM.zip parents size

solve :: [Vec3] -> Int
solve vecs = product $ take 3 rootSizes
 where
  (parents, sizes) = solve' vecs
  roots = map snd $ filter (uncurry (==)) $ V.toList $ V.indexed parents
  rootSizes = sortOn Down $ map (sizes V.!) roots

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . solve
      . map parseVec3
      . T.lines
      . T.strip
