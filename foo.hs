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

-- Values from 400 Byte binary header
data BinaryHeader = BinaryHeader { numTraces :: Int -- Bytes 3213 - 3214
                 , numAuxTraces :: Int -- Bytes 3215 - 3216
                 , sampleInterval :: Float -- Bytes: 3217 - 3218
                 , numSamples :: Int -- Bytes: 3221 - 3222
                 , sampleFormat :: Int -- Bytes: 3225 - 3226
} deriving(Show)

-- Values from 240 Byte trace header
data TraceHeader = TraceHeader { traceNum :: Int -- Bytes 1 - 4
                 , traceNumSegy :: Int -- Byte 5 - 8
} deriving(Show)

data Output = Output [BL.ByteString] BinaryHeader [TraceHeader]


getTextHeaderLine,getTextFileHeader :: Get BL.ByteString
getTextHeaderLine = convert "IBM1047" "UTF8" <$> getLazyByteString 80
getTextFileHeader = getLazyByteString 3200

getTextHeader :: Get [BL.ByteString]
getTextHeader = replicateM 40 getTextHeaderLine
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
-- should be a better way?              <------------------------   look below

    return $ BinaryHeader (fromIntegral numTraces) (fromIntegral numAuxTraces) 
                          (fromIntegral sampleInterval / 1000) (fromIntegral numSamples) 
                          (fromIntegral sampleFormat)
                          

getWord16toIntegral = getWord16be >>= return . fromIntegral           --   <------------------------

getBinHeader2 = BinaryHeader <$> (skip 12 
                              *> getWord16toIntegral)                                -- numTraces
                             <*> getWord16toIntegral                                -- numAuxTraces
                             <*> (getWord16be >>= return . (/1000) . fromIntegral)  -- sampleInterval
                             <*> (skip 12 
                              *> getWord16toIntegral)                                -- numSamples
                             <*> (skip 2
                              *> getWord16toIntegral)                                -- sampleFormat
                             <*  skip (400-26)
 --or to have truly one line:
w2Int = getWord16toIntegral
w2Intdiv1000 = getWord16be >>= return . (/1000) . fromIntegral
getBinHeader3 = BinaryHeader <$> (skip 12 *> w2Int) <*> w2Int <*> w2Intdiv1000 <*> (skip 12 *> w2Int) <*> (skip 2 *> w2Int) <* skip (400-26)

-- EXERCISE :   convert this to applicative style                      <---------------------------
getTraceHeader :: Get TraceHeader
getTraceHeader = do
    traceNum <- getWord32be
    traceNumSegy <- getWord32be
    rest <- getLazyByteString (240 - 8)
    return $ TraceHeader (fromIntegral traceNum) (fromIntegral traceNumSegy)


getSEGY :: Get Output  
getSEGY = do
    header <- getTextHeader
    bheader <- getBinHeader

    let nTraces = numTraces bheader        --  <------------------------------
-- FIXME
-- use numTraces :: BinaryHeader ..
-- How to access it??
   --let l = [1..numTraces :: bheader]
  --  concatMap getTraceHeader [1..numTraces]
    --forM [1..numTraces] getTraceHeader
    -- FIXME: Create a list of TraceHeader data types


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
    
    return $ Output header bheader []

main :: IO()
main = do
    orig <- BL.readFile "test01.segy"

    -- FIXME:
    --This "feels" like a non-haskell way of doing it by two let statements in
    -- a row??
    
    -- I placed the conversion into the getTextHeaderLine function         <------------------------
--    let ebcdic = convert "IBM1047" "UTF8" (BL.take 3200 orig)            <------------------------
  --  let content = BL.append ebcdic (BL.drop 3200 orig)                  <------------------------

    let Output h bh theaders = runGet getSEGY orig             --        <------------------------
    BC.putStr $ BC.unlines h
    putStrLn $ show (bh)
