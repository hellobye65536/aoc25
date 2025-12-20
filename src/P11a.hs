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
solve adj = lookupCnt "out"
 where
  nodes = "out" : map fst adj
  preds = HM.fromListWith (++) [(v, [u]) | (u, vs) <- adj, v <- vs]
  cnts = HM.fromList $ map ((,) <*> countPreds) nodes ++ [("you", 1)]
  lookupCnt n = HM.findWithDefault 0 n cnts
  countPreds n = sum $ map lookupCnt $ HM.findWithDefault [] n preds

main :: IO ()
main =
  T.interact $
    (<> "\n")
      . T.show
      . solve
      . map parse
      . T.lines
      . T.strip
