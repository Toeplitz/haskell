import qualified Data.Traversable as Tr

data Test = Test { desc  :: String
                 , value :: Int
} deriving Show

data Data = Data { foo :: Test
                 , bar :: Test
} deriving Show

exampleData = Data (Test "foo" 1)
                   (Test "bar" 2)

instance Tr.Traversable Data where
    traverse f = -- how to implement this?


main = do
  print $ foo exampleData
  -- Traverse exampleData, printing "foo" and "bar"

