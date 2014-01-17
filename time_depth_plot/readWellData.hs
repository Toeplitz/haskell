import Data.Maybe
import Control.Applicative
import Data.List.Split


parseCheckShot :: String -> [String]
parseCheckShot x = do
    let xs = words x
    [xs !! 2, xs !! 3, xs !! 5]

getValues :: Int -> [String] -> String
getValues n xs = xs !! n


readAbsFloat :: [String] -> [Float]
readAbsFloat = map (abs . read)


main :: IO()
main = do
    content <- readFile "well_checkshot_ascii.txt"
    let xs = parseCheckShot <$> drop 17 (lines content)
    let z = readAbsFloat $ (getValues 0) <$> xs
    let twt = readAbsFloat $ (getValues 1) <$> xs
    let well = (getValues 2) <$> xs
    print z
    print twt
    print well

