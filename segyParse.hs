{-# OPTIONS -XBangPatterns -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

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
import qualified Data.Traversable as T
import qualified Data.Foldable as F


-- Values from 400 Byte binary header
data BinaryHeader = BinaryHeader { numTraces :: Int -- Bytes 3213 - 3214
                 , numAuxTraces :: Int -- Bytes 3215 - 3216
                 , sampleInterval :: Int -- Bytes: 3217 - 3218
                 , numSamples :: Int -- Bytes: 3221 - 3222
                 , sampleFormat :: Int -- Bytes: 3225 - 3226
} deriving(Show)




data Trace = Trace { traceHeader :: TraceHeader
           , dataPoints :: [Float]
} deriving(Show)


data Trace2 = Trace2 { traceHeader2 :: TraceHeader2 ByteLoc
           , dataPoints2 :: [Float]
} deriving(Show)


data Output = Output { ebcdic :: [BL.ByteString]
                     , binaryHeader :: BinaryHeader 
                     , trace :: [Trace2]
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


data ByteLoc = ByteLoc 
  { description :: String 
  , startByte   :: Int
  , endByte     :: Int
  , value       :: Maybe Int
  } 


instance Show ByteLoc where
    show f = (description f) ++ "\t:\t" ++ val
               where val = case value f of
                             Just x -> show x
                             Nothing -> "not set"


data TraceHeader2 t = TraceHeader2 
  { traceNumLine2         :: t -- Bytes 1 - 4
  , traceSeqNum2          :: t -- Bytes 5 - 8
  , origFieldRecordNum2   :: t -- Bytes 9 - 12
  } deriving (Functor, T.Traversable, F.Foldable, Show)


defaultTraceHeader :: TraceHeader2 ByteLoc
defaultTraceHeader = TraceHeader2 
  { traceNumLine2        = ByteLoc "Trace sequence number within line"             1 4  Nothing
  , traceSeqNum2         = ByteLoc "Trace sequence number within SEG Y file"       5 8  Nothing 
  , origFieldRecordNum2  = ByteLoc "Trace sequence number within SEG Y file"       9 12 Nothing 
  }


-- Values from 240 Byte trace header
data TraceHeader = TraceHeader { traceNumLine         :: Int -- Bytes 1 - 4
                               , traceSeqNum          :: Int -- Bytes 5 - 8
                               , origFieldRecordNum   :: Int -- Bytes 9 - 12
                               , traceNumFieldRec     :: Int -- Bytes 13 - 16
                               , energySourcePointNum :: Int -- Bytes 17 - 20
                               , ensembleNum          :: Int -- Bytes 21 - 24
                               , traceNumEnsemble     :: Int -- Bytes 25 - 28
                               , traceIdCode          :: Int -- Bytes 29 - 30
                               , numVertSumTraces     :: Int -- Bytes 31 - 32
                               , numHorStackTraces    :: Int -- Bytes 33 - 34
                               , datUse               :: Int -- Bytes 35 - 36
                               , distSourcePoint      :: Int -- Bytes 37 - 40
                               , receiverGrpElev      :: Int -- Bytes 41 - 44
                               , surfElevSource       :: Int -- Bytes 45 - 48
                               , sourceDepth          :: Int -- Bytes 49 - 52
                               , datumElevReceiver    :: Int -- Bytes 53 - 56
                               , datumElevSource      :: Int -- Bytes 57 - 60
                               , waterDepthSource     :: Int -- Bytes 61 - 64
                               , waterDepthGroup      :: Int -- Bytes 65 - 68
                               , scalarDepths         :: Int -- Bytes 69 - 70
                               , scalarCoords         :: Int -- Bytes 71 - 72
                               , sourceCoordX         :: Int -- Bytes 73 - 76
                               , sourceCoordY         :: Int -- Bytes 77 - 80
                               , groupCoordX          :: Int -- Bytes 81 - 84
                               , groupCoordY          :: Int -- Bytes 85 - 88
                               , coordUnit            :: Int -- Bytes 89 - 90
} deriving Show


getTraceHeader2 :: ByteLoc -> Get ByteLoc
getTraceHeader2 f = do
    d <- getWord32toIntegral
    return $ f { value = Just d } 


getTraceHeader :: Get TraceHeader
getTraceHeader = TraceHeader <$> getWord32toIntegral -- traceNumLine
                             <*> getWord32toIntegral -- traceSeqNumline
                             <*> getWord32toIntegral -- origFieldRecordNum 
                             <*> getWord32toIntegral -- traceNumFieldRec 
                             <*> getWord32toIntegral -- energySourcePointNum
                             <*> getWord32toIntegral -- ensembleNum
                             <*> getWord32toIntegral -- traceNumEnsemble
                             <*> getWord16toIntegral -- traceIdCode
                             <*> getWord16toIntegral -- numVertSumTraces
                             <*> getWord16toIntegral -- numHorStackTraces
                             <*> getWord16toIntegral -- dataUse
                             <*> getWord32toIntegral -- distSourcePoint
                             <*> getWord32toIntegral -- receiverGrpElev
                             <*> getWord32toIntegral -- surfElevSource
                             <*> getWord32toIntegral -- sourceDepth
                             <*> getWord32toIntegral -- datumElevReceiver
                             <*> getWord32toIntegral -- datumElevSource
                             <*> getWord32toIntegral -- waterDepthSource
                             <*> getWord32toIntegral -- waterDepthGroup
                             <*> getWord16toIntegral -- scalarDepths
                             <*> getWord16toIntegral -- scalarCoords
                             <*> getWord32toIntegral -- sourceCoordX
                             <*> getWord32toIntegral -- sourceCoordY
                             <*> getWord32toIntegral -- groupCoordX
                             <*> getWord32toIntegral -- groupCoordY
                             <*> getWord16toIntegral -- coordUnit
                             <* skip (240 - 90)



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


getTrace :: Int -> Int-> Get Trace2
getTrace numSamples sampleFormat = do
  --    th <- getTraceHeader
      th <- T.mapM getTraceHeader2 defaultTraceHeader
      x <- getTraceData numSamples 
      case sampleFormat of 
        5 -> return $ Trace2 th $ wordToFloat <$> x
        1 -> return $ Trace2 th $ wordToFloat . ibmToIeee754 <$> x
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


printBinaryHeader :: Output -> String
printBinaryHeader output = Pr.ppShow (binaryHeader output)


printTraceHeader trace = T.mapM print val
                          where val = traceHeader2 trace


printTraces num output = do
  let traces = take num (trace output)
  printTraceHeader <$> traces


readSegyLazy :: FilePath -> IO BL.ByteString
readSegyLazy file = BL.readFile file


--parseFile opts output = printBinaryHeader output
--  when (optPrintEbcdic opts) $ printEbcdic output
  --when (optPrintBinary opts) $ 
  --when (isJust $ optPrintTraces opts) $ printTraces num output
  --  where num = read (fromJust $ optPrintTraces opts) :: Int


main :: IO()
main = do
  args <- getArgs
  (opts, strs) <- compilerOpts args
  when (null strs) $ error header

  streams <- mapM readSegyLazy strs
  let (x:xs) = runGet getSEGY <$> streams

  print $ binaryHeader x

  T.mapM print defaultTraceHeader
  return ()

  --putStrLn . Pr.ppShow $ strs
  --putStrLn . Pr.ppShow $ opts
