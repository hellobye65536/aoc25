{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Monad (filterM, guard)
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Unboxed qualified as V

parseDecimal :: T.Text -> Int
parseDecimal = fst . fromRight (error "bad decimal") . T.decimal

debracket :: T.Text -> T.Text
debracket = T.dropEnd 1 . T.drop 1

parseDecimalList :: T.Text -> [Int]
parseDecimalList = map parseDecimal . T.split (== ',')

parse :: T.Text -> ([[Int]], V.Vector Bool)
parse tx = (buttons, lights)
 where
  broken = map debracket $ T.split (== ' ') tx
  lights = V.fromList $ map (== '#') $ T.unpack $ head broken
  buttons = map parseDecimalList $ tail $ init broken

solve :: ([[Int]], V.Vector Bool) -> Int
solve (buttons, lights) = minimum $ do
  let len = V.length lights
  pressed <- filterM (const [False, True]) buttons
  let resultLights =
        V.accum
          (const . not)
          (V.replicate len False)
          $ map (,())
          $ concat pressed
  guard $ lights == resultLights
  pure $ length pressed

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . sum
      . map (solve . parse)
      . T.lines
      . T.strip
