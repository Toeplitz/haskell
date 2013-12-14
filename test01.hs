--
-- Tutorials from:
-- http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/

--
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

list = [2, 3, 4, 10]

main = do
  print (doubleMe 2.0)
  (print . doubleMe) 2.0
  print $ doubleMe 2.0
  print $ evenSum list
  print $ evenSum []
  putStrLn "-------------"
  print $ filter even list
  print $ evenSum' list
