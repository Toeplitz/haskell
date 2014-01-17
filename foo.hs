{-# OPTIONS -XBangPatterns -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}

import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Int
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
           , dataPoints :: [Float]
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


-- Convert IBM floating point to IEEE754 format, using only integer
-- bit shifting operations. Endianness is assumed to already have been
-- handled, we are using native endinanness here.
--
-- This is based on CWP-SU's ibm_to_float in
-- 43R3/src/su/main/data_conversion/segyread.c (not part of this code
-- repository).
--
ibmToIeee754 :: Word32 -> Word32
ibmToIeee754 0 = 0
ibmToIeee754 from
  | t1 > 254  = let !maxmag = sign .|. 0x7f7fffff in maxmag
  | t1 <= 0   = 0
  | otherwise = let !v = sign .|. exp' .|. frac in v
  where
    sign = 0x80000000 .&. from
    exp' = fromIntegral t1 `unsafeShiftL` 23
    frac = 0x007fffff .&. fmant1
    (fmant1, t1) = iter fmant0 t0
    fmant0 = 0x00ffffff .&. from
    -- t values may end up negative, so we must treat them as Int32
    -- instead of Word32.
    t0 = ((0x7f000000 .&. fromIntegral from) `unsafeShiftR` 22) - 130 :: Int32
    iter fmant t
      | 0x00800000 .&. fmant == 0 = iter fmant' t'
      | otherwise                 = (fmant, t)
      where
        !fmant' = fmant `unsafeShiftL` 1
        !t'     = t - 1


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
      let ibmvec = fmap (wordToFloat . ibmToIeee754) x
      return $ Trace th ibmvec


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
    --orig <- BL.readFile "test_200x200x50_cube_ieee.segy"
    orig <- BL.readFile "test_200x200x50_cube_ibm.segy"
    --orig <- BL.readFile "Avenue.sgy"
    --orig <- BL.readFile "test02.segy"

    let Output h bh theaders = runGet getSEGY orig 
    BC.putStr $ BC.unlines h
    putStrLn $ Pr.ppShow (bh)
    putStrLn . Pr.ppShow $ map traceHeader (take 3 theaders)
    putStrLn . Pr.ppShow $ map dataPoints (take 3 theaders)
