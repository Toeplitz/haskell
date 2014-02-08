import Control.Monad
import Control.Applicative
import qualified Text.Show.Pretty as Pr


data Foo = Foo
  { fooMin :: Float
  , fooMax :: Float
  , fooSum :: Float
  } deriving Show


getLocalFoo :: [Float] -> Foo
getLocalFoo x = Foo a b c
  where
    a = minimum x
    b = maximum x
    c = sum x

getGlobalFoo :: [Foo] -> Foo
getGlobalFoo x = Foo a b c 
  where
    a = minimum $ fmap fooMin x
    b = maximum $ fmap fooMax x
    c = sum $ fmap fooSum x


main :: IO()
main = do
  let numItems = 4000
  let numLists = 100

  -- Create an infinite list of lists of floats, x is [[Float]]
  let x = take numLists $ repeat [1.0 .. numItems] 

  -- Print two first elements of each item
  print $ take 2 (map (take 2) x)

  -- First calculate local min/max/sum for each float list 
  -- then calculate the global min/max/sum based on the results.
  print . getGlobalFoo $ fmap getLocalFoo x


