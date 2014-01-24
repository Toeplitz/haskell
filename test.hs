{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fl
import Control.Monad
import Control.Applicative


data Test = Test { desc  :: String
                 , value :: Int
} 


data Data t = Data { foo :: t 
                   , bar :: t       
                   , baz :: t
} deriving (Functor, Tr.Traversable, Fl.Foldable)


exampleData = Data { foo = Test "Foo" 1 
                   , bar = Test "Bar" 2
                   , baz = Test "Baz" 3
} 

instance Show Test where
  show f = (desc f) ++ ": " ++ (show $ value f)


--instance Functor Data where
--  fmap = Tr.fmapDefault

--instance Fl.Foldable Data where
--  foldMap = Tr.foldMapDefault

--instance Tr.Traversable Data where
--    traverse f (Data a a') = Data <$> f a <*> f a'
--  
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--

ms :: Test 
ms x =  "lol"

main :: IO()
main = do
  --putStrLn $ show exampleData
  Tr.traverse print exampleData
  Tr.traverse (ms) exampleData

-- OBJECTIVE:
-- Traverse exampleData, printing "foo" and "bar"

