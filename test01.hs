import Data.List

--
-- Tutorials from:
-- http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/
--
-- Idea:
-- Write a SEGY haskell module


evenSumFold' :: Integral a => [a] -> a
evenSumFold' = foldl' (+) 0 . (filter even)

evenSumFold :: Integral a => [a] -> a
evenSumFold l = foldl' mysum 0 (filter even l)
  where mysum acc value = acc + value


-- Testing higher order functions
evenSum' :: Integral a => [a] -> a
evenSum' l = mysum 0 (filter even l)
  where
    mysum n [] = n
    mysum n (x:xs) = mysum(n+x) xs

--
--
-- Pattern matching, if, then else
evenSum :: Integral a => [a] -> a
evenSum l = accumSum 0 l
  where 
    accumSum n [] = n
    accumSum n (x:xs) =
      if even x
        then accumSum (n+x) xs
        else accumSum n xs

-- Demo: Type class Num 
doubleMe :: Num a => a -> a
doubleMe x = x + x 

martin :: Num a => a -> a -> a
martin x y = x + y

list = [2, 3, 4, 10]

main = do
  print (doubleMe 2.0)
  (print . doubleMe) 2.0
  print $ doubleMe 2.0
  putStrLn "---- Testing evenSum"
  print $ evenSum list
  print $ evenSum []
  print $ evenSum' list
  print $ evenSumFold list
  print $ evenSumFold' list
  putStrLn "---- Testing foldl"
  print $ foldl martin 0 list
  print $ foldl (+) 0 list
