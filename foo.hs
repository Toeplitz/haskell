{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}

import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Control.Monad
import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word

-- Requirements:
-- print text header
-- print binary header values
-- print trace numbers f
-- print trace sample values by trace number
--
-- Make use of Binary.Get monad?
-- http://hackage.haskell.org/package/binary-0.7.1.0/docs/Data-Binary-Get.html

getTextHeaderLine,getTextFileHeader,getTraceHeader :: Get BL.ByteString
getTextHeaderLine = getLazyByteString 80

getTextFileHeader = getLazyByteString 3200
getTraceHeader = getLazyByteString 240

getTextHeader :: Get [BL.ByteString]
getTextHeader = replicateM 40 getTextHeaderLine

data Output = Output [BL.ByteString] Word16

getBinHeader :: Get Word16
getBinHeader = do
    begining <- getLazyByteString 16
    sampleInterval <- getWord16be
    rest <- getLazyByteString 382
    return sampleInterval

getSEG :: Get Output  
getSEG = do
    header <- getTextHeader
    sampleI <- getBinHeader
    
    -- somehow extract numTraces
    -- forM [1..numTraces] getTrace
    --     getTrace will get both the header and data
    --     getTrace = do
    --           header <- getTraceHeader
    --           data <- getData
    --            return (header,data)
    --     getData = do 
    --           forM [1..numberOfDataPoints] getDataPoint
    --     getDataPoint = do
     --          getFloat or getVector, etc.
     -- you get the idea. You create a hierarchy to get every piece of data you need.    
    
    return $ Output header sampleI

main :: IO()
main = do
    con <- BL.readFile "test02.segy"
    let contents = convert "IBM1047" "UTF8" con

    let Output h bh = runGet getSEG contents   
    BC.putStr $ BC.unlines h
    putStrLn $ show bh
