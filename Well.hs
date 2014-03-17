-- Parsing checkshot data exported from Petrel
--
--

module Well where
import Text.ParserCombinators.Parsec

data Well = Well { wellX    :: Float
                 , wellY    :: Float
                 , wellZ    :: Float
                 , wellTWT  :: Float
                 , wellMD   :: Float
                 , wellName :: String
} deriving Show

getWellX :: [Well] -> String -> [Well]
getWellX xs name = filter (\x -> wellName x == name) xs

plainValue :: Parser String
plainValue = many (noneOf " \n")

quotedValue :: Parser String
quotedValue = do
    _ <- char '"'
    content <- many (noneOf "\"")
    _ <- char '"'
    return content

wellLine :: Parser Well
wellLine = do
  x <- plainValue
  spaces
  y <- plainValue
  spaces
  z <- plainValue
  spaces
  twt <- plainValue
  spaces
  md <- plainValue
  spaces
  name <- quotedValue
  return $ Well (read x) (read y) (read z) (read twt) (read md) name

--wellParse :: String -> IO ()
wellParse = undefined
--wellParse input = case parse wellLine "(test)" input of
 --           Left err  -> []
 --           Right res -> res
