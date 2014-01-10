{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}

import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import Data.Binary.Get
import Control.Monad
import Control.Applicative
import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import qualified Text.Show.Pretty as Pr
 

-- Values from 400 Byte binary header
data BinaryHeader = BinaryHeader { numTraces :: Int -- Bytes 3213 - 3214
                 , numAuxTraces :: Int -- Bytes 3215 - 3216
                 , sampleInterval :: Int -- Bytes: 3217 - 3218
                 , numSamples :: Int -- Bytes: 3221 - 3222
                 , sampleFormat :: Int -- Bytes: 3225 - 3226
} deriving(Show)


-- Values from 240 Byte trace header
data TraceHeader = TraceHeader { traceNum :: Int -- Bytes 1 - 4
                 , traceNumSegy :: Int -- Byte 5 - 8
                 , traceIdCode :: Int -- Byte 29 - 30
} deriving(Show)

data Trace = Trace { traceHeader :: TraceHeader
           , dataPoints :: [Word32]
} deriving(Show)


data Output = Output [BL.ByteString] BinaryHeader [Trace]


getTextHeaderLine,getTextFileHeader :: Get BL.ByteString
getTextHeaderLine = convert "IBM1047" "UTF8" <$> getLazyByteString 80
getTextFileHeader = getLazyByteString 3200


getTextHeader :: Get [BL.ByteString]
getTextHeader = replicateM 40 getTextHeaderLine

getWord16toIntegral :: Get Int
getWord16toIntegral = getWord16be >>= return . fromIntegral -- Inject Num into the Get monadic type
getWord32toIntegral :: Get Int
getWord32toIntegral = getWord32be >>= return . fromIntegral -- Inject Num into the Get monadic type


infixl 5 *>>
(*>>) :: Applicative f => f a -> f b -> f b
(*>>) = (*>)


getBinHeader :: Get BinaryHeader
getBinHeader = BinaryHeader <$> skip 12 
                            *>> getWord16toIntegral -- numTraces
                            <*> getWord16toIntegral -- numAuxTraces
                            <*> getWord16toIntegral -- sampleInterval
                            <*> skip 2
                            *>> getWord16toIntegral -- numSamples
                            <*> skip 2
                            *>> getWord16toIntegral -- sampleFormat
                            <*  skip (400-26)


getTraceHeader :: Get TraceHeader
getTraceHeader = TraceHeader <$> getWord32toIntegral -- traceNum
                             <*> getWord32toIntegral -- traceNumSegy
                             <*> skip 16
                             *>> getWord16toIntegral -- traceIdCode
                             <* skip (240 - 26)


-- Based on this python implementation:
-- http://stackoverflow.com/questions/7125890/python-unpack-ibm-32-bit-float-point
--
ibm2ieee :: Word32 -> Float
ibm2ieee ibm  = do
    let sign = (ibm `shiftL` 31) .&. 0x01
    let exponent = (ibm `shiftL` 24) .&. 0x7f
    --let mantissa = (ibm .&. 0x00ffffff) / 1
    --let sign = ibm `shiftL` 31 & 0x01
    0.1


getData :: Get Word32
getData = getWord32be

getTraceData :: Int -> Get [Word32]
getTraceData numSamples = do
    val <- forM [1 .. 4] $ \func -> do
      getWord32be
    return $ val

getTrace :: BinaryHeader -> Get Trace 
getTrace bh = do
      th <- getTraceHeader
      x <- getTraceData $ numSamples bh
      return $ Trace th x


getSEGY :: Get Output  
getSEGY = do
    header <- getTextHeader
    bheader <- getBinHeader

    -- FIXME: Can I run mapM instead or forM without using a lambda function?
    -- trace <- forM [1 .. numTraces bheader] getTrace does not work
    --trace <- forM [1 .. numTraces bheader] $ \func -> do
    trace <- forM [1 .. 3] $ \func -> do
      getTrace bheader

      
    --forM [1..numTraces] getTraceHeader
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
    
    return $ Output header bheader trace

main :: IO()
main = do
    --orig <- BL.readFile "WD_3D.sgy"
    orig <- BL.readFile "Avenue.sgy"
    --orig <- BL.readFile "test02.segy"

    let Output h bh theaders = runGet getSEGY orig 
    BC.putStr $ BC.unlines h
    putStrLn $ Pr.ppShow (bh)
    putStrLn . Pr.ppShow $ map traceHeader (take 3 theaders)
    putStrLn . Pr.ppShow $ map dataPoints (take 3 theaders)
