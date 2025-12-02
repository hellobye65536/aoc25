module Main (main) where

parse :: String -> Int
parse ('L' : xs) = -read xs
parse ('R' : xs) = read xs
parse _ = error "invalid"

main :: IO ()
main =
  interact $
    show
      . length
      . filter (== 0)
      . scanl (\acc x -> (acc + x) `mod` 100) 50
      . map parse
      . lines
