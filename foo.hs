{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}

import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Control.Monad
import Control.Applicative
import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word


-- Questions:
-- Is it not "nice" to create this as a SEGY monad?
--
-- getTextHeader :: SEGY [String]
-- getTextHeader = do
--   blah blah, return as a list of strings?
--
-- I have added FIXME in the code where I have specific issues.
--

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


data BinaryHeader = BinaryHeader { numTraces :: Int -- Bytes 3213 - 3214
                 , numAuxTraces :: Int -- Bytes 3215 - 3216
                 , sampleInterval :: Float -- Bytes: 3217 - 3218
                 , numSamples :: Int -- Bytes: 3221 - 3222
                 , sampleFormat :: Int -- Bytes: 3225 - 3226
} deriving(Show)


data Output = Output [BL.ByteString] BinaryHeader 


getBinHeader :: Get BinaryHeader
getBinHeader = do
-- FIXME: 
-- Should be a better way to do this avoiding the "mid" bind..
--
-- Also it is very repetative, maybe if I have all the variables in the
-- BinaryHeader structure it could be a oneliner?
--
    begining <- getLazyByteString 12
    numTraces <- getWord16be
    numAuxTraces <- getWord16be
    sampleInterval <- getWord16be
    mid <- getLazyByteString 2
    numSamples <- getWord16be
    mid <- getLazyByteString 2
    sampleFormat <- getWord16be
    rest <- getLazyByteString (400 - 26)

-- FIXME:
-- I am converting from Word16 to a number in a very primitive way here,
-- should be a better way?
    return $ BinaryHeader (fromIntegral numTraces) (fromIntegral numAuxTraces) (fromIntegral sampleInterval / 1000) (fromIntegral numSamples) (fromIntegral sampleFormat)

getSEGY :: Get Output  
getSEGY = do
    header <- getTextHeader
    bheader <- getBinHeader

-- FIXME
-- use numTraces :: BinaryHeader ..
-- Extract trace headers in a loop
--


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
    
    return $ Output header bheader

main :: IO()
main = do
    orig <- BL.readFile "test01.segy"

    -- FIXME:
    --This "feels" like a non-haskell way of doing it by two let statements in
    -- a row??
    let ebcdic = convert "IBM1047" "UTF8" (BL.take 3200 orig)
    let content = BL.append ebcdic (BL.drop 3200 orig)

    let Output h bh = runGet getSEGY content
    BC.putStr $ BC.unlines h
    putStrLn $ show (bh)
