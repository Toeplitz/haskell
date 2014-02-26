-- Parsing checkshot data exported from Petrel
--
--

module Well where
--import Control.Monad
import Text.ParserCombinators.Parsec

data Well = Well { wellX    :: Float
                 , wellY    :: Float
                 , wellZ    :: Float
                 , wellTWT  :: Float
                 , wellMD   :: Float
                 , wellName :: String
} deriving Show


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

wellParse :: String -> IO ()
wellParse input = case parse wellLine "(test)" input of
            Left err  -> print err
            Right res -> print res
