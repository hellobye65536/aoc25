{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as VM
import Control.Monad.ST (runST)

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

{- HLINT ignore solve "Use infix" -}
solve :: [Vec3] -> Int
solve vecs_ = runST $ do
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
    go [] ret = pure ret
    go ((u, v) : edges) ret = do
      unioned <- union u v
      if unioned
        then
          let (ux, _, _) = vecs V.! u
              (vx, _, _) = vecs V.! v
           in go edges (ux * vx)
        else go edges ret
    edges =
      sortOn
        (uncurry dist . join bimap (vecs V.!))
        [(u, v) | u <- [0 .. vl - 1], v <- [u + 1 .. vl - 1]]
  go edges invalid

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . solve
      . map parseVec3
      . T.lines
      . T.strip
