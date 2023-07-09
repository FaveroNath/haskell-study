module Main (main) where

soma a b = a + b


fat 0 = 1
fat n = n * fat (n-1)

main :: IO ()
main = do
  print $ fat 3
