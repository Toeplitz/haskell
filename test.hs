import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fl
import Control.Monad
import Control.Applicative

data Test = Test { desc  :: String
                 , value :: Int
} 


data Data t = Data { foo :: t 
                   , bar :: t       
} 

exampleData = Data { foo = Test "foo" 1 
                   , bar = Test "Bar" 2
}

instance Show Test where
  show f = (desc f) ++ ": " ++ (show $ value f)

instance (Show a) => Show (Data a) where
  show f = show (foo f)

instance Functor Data where
  fmap = Tr.fmapDefault

instance Fl.Foldable Data where
  foldMap = Tr.foldMapDefault

instance Tr.Traversable Data where
    traverse f = Data f  -- Try to show a Test entry inside the Data structure

--  
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--

main = do
  putStrLn $ show exampleData
  Tr.traverse (putStrLn show) exampleData

-- OBJECTIVE:
-- Traverse exampleData, printing "foo" and "bar"

