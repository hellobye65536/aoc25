{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Data.HashMap.Lazy qualified as HM
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse :: T.Text -> (T.Text, [T.Text])
parse tx = case T.words $ T.strip tx of
  (u : vs) -> (T.dropEnd 1 u, vs)
  _ -> error ("bad line" <> T.unpack tx)

solve :: [(T.Text, [T.Text])] -> Int
solve adj =
  lookupCnt svrCnts "fft" * lookupCnt fftCnts "dac" * lookupCnt dacCnts "out"
    + lookupCnt svrCnts "dac" * lookupCnt dacCnts "fft" * lookupCnt fftCnts "out"
 where
  nodes = "out" : map fst adj
  preds = HM.fromListWith (++) [(v, [u]) | (u, vs) <- adj, v <- vs]
  lookupCnt table n = HM.findWithDefault 0 n table
  svrCnts = makeCnts "svr"
  fftCnts = makeCnts "fft"
  dacCnts = makeCnts "dac"
  makeCnts start = let cnts = HM.fromList $ map ((,) <*> countPreds cnts) nodes ++ [(start, 1)] in cnts
  countPreds self n = sum $ map (lookupCnt self) $ HM.findWithDefault [] n preds

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . solve
      . map parse
      . T.lines
      . T.strip
