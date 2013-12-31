import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary.Get
import Data.List
import Data.Word
import Data.Bits
import Data.Char
import System.Environment

--parseCmd :: String -> [String] -> String
--parseCmd _ [] = "empty"
--parseCmd "read" xs = head (readSegy . head $ xs)
--parseCmd _ xs = "wrong arguments, list length: " ++ show (length xs)


--readSegy :: String -> [Char]
--readSegy file = do
--  contents <- B.readFile file
--  B.unpack contents

-- http://hackage.haskell.org/package/binary-0.7.1.0/docs/Data-Binary-Get.html
-- http://www.seg.org/documents/10161/77915/seg_y_rev1.pdf

--readTextHeader :: BL.ByteString -> String
--readTextHeader a = U.unpack a

decodeHeader :: Get (Word16, Word16, Word16)
decodeHeader = do
  test <- getWord16be
  test2 <- getWord16be
  test3 <- getWord16be
  return (test, test2, test3)

main = do
  (cmd:args) <- getArgs
  --print $ parseCmd cmd args
  contents <- BL.readFile "vel_z6.25m_x12.5m_exact.segy"
  --print $ readTextHeader (BL.take 3200 contents)
  print $ (BL.unpack (BL.take 3200 contents))
  --BL.putStr $ (BL.take 3200 contents)
  
