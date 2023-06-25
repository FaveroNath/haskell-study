module Main (main) where

soma a b = a + b

main :: IO ()
main = do
  print $ soma 1 2
