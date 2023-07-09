module Main (main) where

ou False False = False
ou _ _ = True

ee a b = if a then 
              if b then True else b
          else a
ee' a b = if a then b else a

mc91 :: Integral a => a -> a
mc91 n = if n > 10 then n - 10 else (mc91.mc91) (n + 11)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x [y] = x == y
elem' y (x:xs) | y == x = True
               | otherwise = elem' y xs

mdc :: Int -> Int -> Int
mdc a b | a == b = a
        | a < b = mdc a (b-a)
        | otherwise = mdc b (a-b)

mdc' 0 b = b
mdc a 0 = a
mdc' a b = mdc b (a `rem` b)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' y (x:xs) = x : y : intersperse' y xs 

main :: IO ()
main = do
  print (ee True False)
  print (ee True True)
  print (ee' True True)
  print (ee' True False)
  print (mc91 11)
  print (elem' 1 [1,2,3])
  print (elem' 1 [])
  print (elem' 1 [2,3,6,7,1,6,7])
  print (elem' 8 [1,2])
  print (elem' 1 [2])
  print (mdc 29 13)
  print (concat' [[1, 2, 3], [4, 5], [6, 7, 8, 9]])
  print (intersperse' '-' "abcde")
