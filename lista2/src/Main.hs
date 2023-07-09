module Main (main) where

penultimo [] = error "lista vazia"
penultimo [_] = error "lista unitaria"
penultimo [x, _] = x
penultimo (_:xs) = penultimo xs

maxLocal :: Ord a => [a] -> [a]
maxLocal [] = []
maxLocal [_] = []
maxLocal [_,_] = []
maxLocal (x:y:z:zs) | x < y && y > z = y : maxLocal (y:z:zs)
                    | otherwise = maxLocal (y:z:zs)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs)= x + sum' xs

divs' :: Integral a => a -> [a]
divs' n = [d | d <- [1..n-1], mod n d == 0]

perfeitos :: Integral a => a -> [a]
perfeitos  n = [k | k <- [1..n], sum' (divs' k)  == k]

produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar xs ys | length xs /= length ys  = error "Eca"
                     | otherwise = sum' [xs !! i * ys !! i | i <- [0..length xs - 1]] 

palindromo :: Eq a  => [a] -> Bool
palindromo [] = True
palindromo [_] = True
palindromo (x:xs) = x == last xs && palindromo (init xs)


-- qSortBy :: Ord b => (a -> b) -> [a] -> [a]
-- qSortBy _ [] = []
-- qSortBy k (x:xs) = qSortBy k ls ++ [x] ++ qsortBy k bs 
--   where ls = [y | y <= xs, k y <= k x]
--         os = [y | y <- xs, k y > k x]

-- ordenaListas :: [[a]] -> [[a]]
-- ordenaListas = qSortBy length

digitoRev :: Int -> [Int]
digitoRev x   | x < 10 = [x]
              | otherwise = x `rem` 10 : digitoRev (x `quot` 10)

digitoNaOrdem :: Int -> [Int]
digitoNaOrdem x = reverse (digitoRev x)

dobraAlternado :: [Int] -> [Int]
dobraAlternado [x] = [x]
dobraAlternado [x, y] = [x, 2 * y]
dobraAlternado (x:y:xs) = x : 2 * y : dobraAlternado xs

somaDigitos :: [Int] -> Int
somaDigitos [] = 0
somaDigitos [x] = x
somaDigitos (x:xs) = x + somaDigitos xs
main :: IO ()
main = do
  print (digitoRev 0)
  print (digitoRev 10)
  print (digitoRev 123)
  print (digitoNaOrdem 123)
  print (dobraAlternado [3,6,5,4,1])
  print (somaDigitos [1,2,3])