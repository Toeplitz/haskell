import Data.Maybe
import Control.Applicative


data Output = Output { owd :: Float
                     , twt :: Float
} deriving (Show)

parseCheckShot :: String -> (Float, Float)
parseCheckShot x = (0.1, 0.2)

    

main :: IO()
main = do
    content <- readFile "well_24_9_1_checkshot_ascii.txt"
    let (xs) = parseCheckShot <$> (lines content)
    print $ length . lines $ content
    print owd
    print twt
