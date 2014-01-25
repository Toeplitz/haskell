{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fl
import Control.Monad
import Control.Applicative
import qualified Text.Show.Pretty as Pr


data Test = Test { desc  :: String
                 , value :: Int
}

instance Show Test where
  show f = (desc f) ++ ": " ++ (show $ value f)


data Data t = Data { foo :: t 
                   , bar :: t       
                   , baz :: t
} deriving (Functor, Tr.Traversable, Fl.Foldable, Show)


exampleData = Data { foo = Test "Foo" 1 
                   , bar = Test "Bar" 2
                   , baz = Test "Baz" 3
} 


test :: Monad m => Test -> m Int
test f = return $ value f * 10


test2 :: Monad m => Test -> m Test
test2 f = return $ f { value = x } where x = value f * 10


main :: IO()
main = do

  putStrLn . Pr.ppShow $ exampleData

  d <- Tr.traverse test exampleData
  putStrLn . Pr.ppShow $ d

  d2 <- Tr.mapM test2 exampleData
  putStrLn . Pr.ppShow $ d2

  d3 <- Tr.traverse test2 exampleData
  putStrLn . Pr.ppShow $ d3

  Tr.mapM print exampleData
  Tr.mapM print d3
  return ()
