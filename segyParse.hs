{-# OPTIONS -XBangPatterns -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}

import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import Data.Bits
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Int
import Control.Monad
import Control.Applicative
import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe, fromJust, isJust)

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


data Output = Output { ebcdic :: [BL.ByteString]
                     , binaryHeader :: BinaryHeader 
                     , trace :: [Trace]
} deriving Show


getTextHeaderLine,getTextFileHeader :: Get BL.ByteString
getTextHeaderLine = convert "IBM1047" "UTF8" <$> getLazyByteString 80
getTextFileHeader = getLazyByteString 3200


getTextHeader :: Get [BL.ByteString]
getTextHeader = replicateM 40 getTextHeaderLine

-- In the segy standard all binary values are defined
-- as big-endian byte ordering.
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



getTraceData :: Int -> Get [Word32]
getTraceData numSamples = do
    val <- forM [1 .. numSamples] $ \func -> do
      getWord32be
    return $ val


getTrace :: Int -> Int-> Get Trace 
getTrace numSamples sampleFormat = do
      th <- getTraceHeader
      x <- getTraceData numSamples 
      case sampleFormat of 
        5 -> return $ Trace th $ wordToFloat <$> x
        1 -> return $ Trace th $ wordToFloat . ibmToIeee754 <$> x
        _ -> error "Error: only ibm floating poins or ieee754 sample formats are supported."


getSEGY :: Get Output  
getSEGY = do
    header <- getTextHeader
    bheader <- getBinHeader

    trace <- forM [1 .. 100] $ \func -> do
      getTrace (numSamples bheader) (sampleFormat bheader)

    return $ Output header bheader trace


data Options = Options
 { optShowVersion :: Bool
 , optPrintEbcdic :: Bool
 , optPrintBinary :: Bool
 , optPrintTraces :: Maybe String
 } deriving Show


defaultOptions    = Options
 { optShowVersion = False
 , optPrintEbcdic = False
 , optPrintBinary = False
 , optPrintTraces = Nothing
 }


options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v'] ["version"]
     (NoArg (\opts -> opts { optShowVersion = True }))
     "show version number"
 , Option ['e'] ["ebcdic"]
     (NoArg (\opts -> opts { optPrintEbcdic = True }))
     "print ebcdic header"
 , Option ['b'] ["binary"]
     (NoArg (\opts -> opts { optPrintBinary = True }))
     "print binary header"
 , Option ['t'] ["trace"]
     (OptArg ((\f opts -> opts { optPrintTraces = Just f }) . fromMaybe "trace") "N")
       "print summary of N first traces"
 ]

header :: String
header =  "Usage: segyParse.hs [OPTION...] files..."


compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))


printEbcdic :: Output -> IO ()
printEbcdic bs = BC.putStrLn $ BC.unlines (ebcdic bs)


printBinaryHeader :: Output -> IO ()
printBinaryHeader bh = putStrLn $ Pr.ppShow (binaryHeader bh)


printTraces :: Int -> Output -> IO ()
printTraces num output = putStrLn . Pr.ppShow $ traceHeader <$> traces
                          where traces = take num (trace output)


readSegyLazy :: FilePath -> IO BL.ByteString
readSegyLazy file = BL.readFile file


parseFile :: Options -> Output -> IO ()
parseFile opts output = do
  when (optPrintEbcdic opts) $ printEbcdic output
  when (optPrintBinary opts) $ printBinaryHeader output
  when (isJust (optPrintTraces opts)) $ printTraces (read (fromJust (optPrintTraces opts)) :: Int) output


main :: IO()
main = do
  args <- getArgs
  (opts, strs) <- compilerOpts args
  when (null strs) $ error header

  orig <- mapM readSegyLazy strs
  let output = (runGet getSEGY) <$> orig
  mapM_ (parseFile opts) output

  putStrLn . Pr.ppShow $ strs
  putStrLn . Pr.ppShow $ opts
