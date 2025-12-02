module Main (main) where

parse :: String -> Int
parse ('L' : xs) = -read xs
parse ('R' : xs) = read xs
parse _ = error "invalid"

step :: (Int, Int) -> Int -> (Int, Int)
step (v, _) x = (m, abs d + corr1 + corr2)
 where
  (d, m) = divMod (v + x) 100
  corr1 = if x < 0 && m == 0 then 1 else 0
  corr2 = if v == 0 && x < 0 then -1 else 0

main :: IO ()
main =
  interact $
    (++ "\n")
      . show
      . sum
      . map snd
      . scanl step (50, 0)
      . map parse
      . lines
