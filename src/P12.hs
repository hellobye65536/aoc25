{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Monad (join, when)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (fromRight)
import Data.List (unsnoc)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

newtype Shape = Shape {shapeTileCount :: Int} deriving (Show)
data Problem = Problem {problemWidth :: Int, problemHeight :: Int, problemShapes :: [Int]} deriving (Show)

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight (error "bad decimal") . T.decimal

parseShape :: [T.Text] -> Shape
parseShape (_ : shape) = Shape {shapeTileCount}
 where
  shapeTileCount = sum $ map (length . filter (== '#') . T.unpack) shape
parseShape _ = error "bad parse"

parseProblem :: T.Text -> Problem
parseProblem tx = case T.split (== ':') tx of
  [size, _ T.:< shapes_] -> Problem {problemWidth = width, problemHeight = height, problemShapes = shapes}
   where
    (width, height) = case T.split (== 'x') size of
      [width_, height_] -> join bimap parseDecimal (width_, height_)
      _ -> error "bad parse"
    shapes = map parseDecimal $ T.split (== ' ') shapes_
  _ -> error "bad parse"

solve :: ([Shape], Problem) -> Bool
solve (shapes, problem) = either id id $ do
  let totalTiles = sum $ zipWith (*) (map shapeTileCount shapes) (problemShapes problem)
      availTiles = problemWidth problem * problemHeight problem
  when (totalTiles > availTiles) $ Left False
  let squareCount = product $ map (`quot` 3) $ [problemWidth, problemHeight] <*> pure problem
      shapeSquares = sum $ problemShapes problem
  when (shapeSquares <= squareCount) $ Left True
  pure $ error "solution not implemented"

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . length
      . filter solve
      . sequenceA
      . bimap (map parseShape) (map parseProblem)
      . fromMaybe (error "bad parse")
      . unsnoc
      . splitWhen T.null
      . T.lines
      . T.strip
