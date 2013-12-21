import Data.List
import System.Environment

parseCmd :: String -> [String] -> String
parseCmd _ [] = "empty"
parseCmd "read" xs = show (readSegy xs)
parseCmd _ xs = "wrong arguments, list length: " ++ show (length xs)


readSegy :: [String] -> [String]
readSegy xs = xs


main = do
  (cmd:args) <- getArgs
  print (parseCmd cmd args)
  
