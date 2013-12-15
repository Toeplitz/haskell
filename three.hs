import Data.List

data BinTree a = Empty 
                | Node a (BinTree a) (BinTree a)
                    deriving (Show)


treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))

l = [1..3]

main = do
  putStrLn "hallo"
  print $ treeFromList l
