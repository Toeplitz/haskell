{-# OPTIONS -XBangPatterns -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

import Codec.Text.IConv (convert)
import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Binary.Get
import Data.Binary.IEEE754 (wordToFloat)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Word (Word32)
import System.Environment
import System.Console.GetOpt
import System.IO

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Text.Show.Pretty as Pr

data ByteLoc = ByteLoc 
  { description :: String 
  , startByte   :: Int
  , endByte     :: Int
  , value       :: Maybe Int
  } 

data BinaryHeader t = BinaryHeader
  { jobIdNum              :: t -- Bytes 3201 - 3204
  , lineNum               :: t -- Bytes 3205 - 3208
  , reelNum               :: t -- Bytes 3209 - 3212
  , numTracesEnsemble     :: t -- Bytes 3213 - 3214
  , numAuxTracesEnsemble  :: t -- Bytes 3215 - 3216
  , sampleInterval        :: t -- Bytes 3217 - 3218
  , sampleIntervalField   :: t -- Bytes 3219 - 3220
  , numSamplesTrace       :: t -- Bytes 3221 - 3222
  , numSamplesTraceField  :: t -- Bytes 3223 - 3224
  , sampleFormat          :: t -- Bytes 3225 - 3226
  , ensembleFold          :: t -- Bytes 3227 - 3228
  , traceSortingcode      :: t -- Bytes 3229 - 3230
  , vertSumCode           :: t -- Bytes 3231 - 3232
  , sweepFreqStart        :: t -- Bytes 3233 - 3234
  , sweepFreqEnd          :: t -- Bytes 3235 - 3236
  , sweepLength           :: t -- Bytes 3237 - 3238
  , sweepTypeCode         :: t -- Bytes 3239 - 3240
  , traceNumSweep         :: t -- Bytes 3241 - 3242
  , sweepTaperStart       :: t -- Bytes 3243 - 3244
  , sweepTapeEnd          :: t -- Bytes 3245 - 3246
  , taperType             :: t -- Bytes 3247 - 3248
  , corrDataTraces        :: t -- Bytes 3249 - 3250
  , binaryGainRecovered   :: t -- Bytes 3251 - 3252
  , ampRecoveryMethod     :: t -- Bytes 3253 - 3254
  , measurementSystem     :: t -- Bytes 3255 - 3256
  , impulseSigPolarity    :: t -- Bytes 3257 - 3258
  , vibPolarityCode       :: t -- Bytes 3259 - 3260
  , unassigned01          :: t -- Bytes 3261 - 3500
  , segyFormatRevNum      :: t -- Bytes 3501 - 3502
  , fixedLenTraceFlag     :: t -- Bytes 3503 - 3504
  , numExtendHeaders      :: t -- Bytes 3505 - 3506
  , unassigned02          :: t -- Bytes 3507 - 3600
  } deriving (Functor, T.Traversable, F.Foldable, Show)


defaultBinaryHeader :: BinaryHeader ByteLoc
defaultBinaryHeader = BinaryHeader
  { jobIdNum              = ByteLoc "Job identification number                                       "   3201 3204 Nothing
  , lineNum               = ByteLoc "Line number                                                     "   3205 3208 Nothing 
  , reelNum               = ByteLoc "Reel number                                                     "   3209 3212 Nothing 
  , numTracesEnsemble     = ByteLoc "Number of data traces per ensemble (mandatory pre-stack)        "   3213 3214 Nothing
  , numAuxTracesEnsemble  = ByteLoc "Numer of auxiliary traces per ensemble (mandatory pre-stack)    "   3215 3216 Nothing
  , sampleInterval        = ByteLoc "Sample interval in microseconds (mandatory)                     "   3217 3218 Nothing
  , sampleIntervalField   = ByteLoc "Sample interval in microseconds of orignal field recording      "   3219 3220 Nothing
  , numSamplesTrace       = ByteLoc "Numer of samples per data trace (mandatory)                     "   3221 3222 Nothing
  , numSamplesTraceField  = ByteLoc "Numer of samples per data trace for original field recording    "   3223 3224 Nothing
  , sampleFormat          = ByteLoc "Data sample format (mandatory)                                  "   3225 3226 Nothing 
  , ensembleFold          = ByteLoc "Ensemble fold (mandatory)                                       "   3227 3228 Nothing
  , traceSortingcode      = ByteLoc "Trace sorting code (mandatory)                                  "   3229 3230 Nothing
  , vertSumCode           = ByteLoc "Vertical sum code                                               "   3231 3232 Nothing
  , sweepFreqStart        = ByteLoc "Sweep frequency at start                                        "   3233 3234 Nothing
  , sweepFreqEnd          = ByteLoc "Sweep frequency at end                                          "   3235 3236 Nothing
  , sweepLength           = ByteLoc "Sweep length                                                    "   3237 3238 Nothing
  , sweepTypeCode         = ByteLoc "Sweep type code                                                 "   3239 3240 Nothing
  , traceNumSweep         = ByteLoc "Trace numer of sweep channel                                    "   3241 3242 Nothing
  , sweepTaperStart       = ByteLoc "Sweep trace taper length at start if tapered                    "   3243 3244 Nothing
  , sweepTapeEnd          = ByteLoc "Sweep trace taper length at end if tapered                      "   3245 3246 Nothing
  , taperType             = ByteLoc "Taper type                                                      "   3247 3248 Nothing
  , corrDataTraces        = ByteLoc "Correlated data traces                                          "   3249 3250 Nothing
  , binaryGainRecovered   = ByteLoc "Binary gain recovered                                           "   3251 3252 Nothing
  , ampRecoveryMethod     = ByteLoc "Amplitude recovery method (mandatory)                           "   3253 3254 Nothing
  , measurementSystem     = ByteLoc "Measurement system                                              "   3255 3256 Nothing
  , impulseSigPolarity    = ByteLoc "Impulse signal polarity                                         "   3257 3258 Nothing
  , vibPolarityCode       = ByteLoc "Vibratory polarity code                                         "   3259 3260 Nothing
  , unassigned01          = ByteLoc "Unassigned                                                      "   3261 3500 Nothing
  , segyFormatRevNum      = ByteLoc "SEG Y Format Revision Number (mandatory)                        "   3501 3502 Nothing
  , fixedLenTraceFlag     = ByteLoc "Fixed length trace flag (mandatory)                             "   3503 3504 Nothing
  , numExtendHeaders      = ByteLoc "Numer of extended headers (mandatory)                           "   3505 3506 Nothing
  , unassigned02          = ByteLoc "Unassigned                                                      "   3507 3600 Nothing
  } 


data TraceHeaderEssential t = TraceHeaderEssential
  { traceNumLine         :: t -- Bytes 1  - 4
  , traceSeqNum          :: t -- Bytes 5  - 8
  }


data TraceHeader t = TraceHeader 
  { traceNumLine         :: t -- Bytes 1  - 4
  , traceSeqNum          :: t -- Bytes 5  - 8
  , origFieldRecordNum   :: t -- Bytes 9  - 12
  , traceNumFieldRec     :: t -- Bytes 13 - 16
  , energySourcePointNum :: t -- Bytes 17 - 20
  , ensembleNumber       :: t -- Bytes 21 - 24
  , traceNumEnsemble     :: t -- Bytes 25 - 28
  , traceIdCode          :: t -- Bytes 29 - 30
  , numVertSumTraces     :: t -- Bytes 31 - 32
  , numHorSumTraces      :: t -- Bytes 33 - 34
  , dataUse              :: t -- Bytes 35 - 36
  , distCenter           :: t -- Bytes 37 - 40
  , receiverGroupElev    :: t -- Bytes 41 - 44
  , surfaceElevSource    :: t -- Bytes 45 - 48
  , sourceDepthSurface   :: t -- Bytes 49 - 52
  , datumElevReceiver    :: t -- Bytes 53 - 56
  , datumElevSource      :: t -- Bytes 57 - 60
  , waterDepthSource     :: t -- Bytes 61 - 64
  , waterDepthReceiver   :: t -- Bytes 65 - 68
  , scalarElev           :: t -- Bytes 69 - 70
  , scalarCoord          :: t -- Bytes 71 - 72
  , sourceCoordX         :: t -- Bytes 73 - 76
  , sourceCoordY         :: t -- Bytes 77 - 80
  , groupCoordX          :: t -- Bytes 81 - 84
  , groupCoordY          :: t -- Bytes 85 - 88
  , coordUnits           :: t -- Bytes 89 - 90
  , unassigned03         :: t
  , nSamplesTraceHeader  :: t -- Bytes 115 - 116
  , sampleIntervalTrace  :: t -- Bytes 117 - 118
  , unassigned04         :: t
  } deriving (Functor, T.Traversable, F.Foldable, Show)

defaultTraceHeader :: TraceHeader ByteLoc
defaultTraceHeader = TraceHeader 
  { traceNumLine          = ByteLoc "Trace sequence number within line (recommended)                 "  1   4   Nothing
  , traceSeqNum           = ByteLoc "Trace sequence number within SEG Y file                         "  5   8   Nothing 
  , origFieldRecordNum    = ByteLoc "Original field record number (recommended)                      "  9   12  Nothing 
  , traceNumFieldRec      = ByteLoc "Trace number within original field record (recommended)         "  13  16  Nothing
  , energySourcePointNum  = ByteLoc "Energy source point number                                      "  17  20  Nothing
  , ensembleNumber        = ByteLoc "Ensemble number                                                 "  21  24  Nothing
  , traceNumEnsemble      = ByteLoc "Trace number within the ensemble                                "  25  28  Nothing
  , traceIdCode           = ByteLoc "Trace identification code                                       "  29  30  Nothing
  , numVertSumTraces      = ByteLoc "Number of vertically summed traces yielding this trace          "  31  32  Nothing
  , numHorSumTraces       = ByteLoc "Number of horizontally stacked traces yielding this trace       "  33  34  Nothing
  , dataUse               = ByteLoc "Data use                                                        "  35  36  Nothing
  , distCenter            = ByteLoc "Distance from center of the source point to the receiver group  "  37  40  Nothing
  , receiverGroupElev     = ByteLoc "Receiver group elevation                                        "  41  44  Nothing
  , surfaceElevSource     = ByteLoc "Surface elevation at source                                     "  45  48  Nothing
  , sourceDepthSurface    = ByteLoc "Source depth below surface                                      "  49  52  Nothing
  , datumElevReceiver     = ByteLoc "Datum elevation at receiver group                               "  53  56  Nothing
  , datumElevSource       = ByteLoc "Datum elevation at source                                       "  57  60  Nothing
  , waterDepthSource      = ByteLoc "Water depth at source                                           "  61  64  Nothing
  , waterDepthReceiver    = ByteLoc "Water depth at group                                            "  65  68  Nothing
  , scalarElev            = ByteLoc "Scalar to be applied to all elevations and depths               "  69  70  Nothing
  , scalarCoord           = ByteLoc "Scalar to be applied to all coordinates                         "  71  72  Nothing
  , sourceCoordX          = ByteLoc "Source coordinate X                                             "  73  76  Nothing
  , sourceCoordY          = ByteLoc "Source coordinate Y                                             "  77  80  Nothing
  , groupCoordX           = ByteLoc "Group coordinate X                                              "  81  84  Nothing
  , groupCoordY           = ByteLoc "Group coordinate Y                                              "  85  88  Nothing
  , coordUnits            = ByteLoc "Coordinate units                                                "  89  90  Nothing 
  , unassigned03          = ByteLoc "....                                                            "  91  114 Nothing
  , nSamplesTraceHeader   = ByteLoc "Number of samples in this trace                                 "  115 116 Nothing
  , sampleIntervalTrace   = ByteLoc "Sample interval for this trace                                  "  117 118 Nothing
  , unassigned04          = ByteLoc "...                                                             "  119 240 Nothing
  }

data Trace = Trace 
  { traceHeader :: TraceHeader ByteLoc
  , dataPoints :: [Float]
  } deriving(Show)


data Output = Output 
  { ebcdic :: [BL.ByteString]
  , binaryHeader :: BinaryHeader ByteLoc
  , traces :: [Trace]
  } 

getTextHeaderLine,getTextFileHeader :: Get BL.ByteString
getTextHeaderLine = convert "IBM1047" "UTF8" <$> getLazyByteString 80
getTextFileHeader = getLazyByteString 3200

getTextHeader :: Get [BL.ByteString]
getTextHeader = replicateM 40 getTextHeaderLine

getLocSizeStr :: Int -> Int -> String
getLocSizeStr x y = case x - y of
                      3 -> " (Int32)"
                      1 -> " (Int16)"
                      _ -> " (unknown)"

instance Show ByteLoc where
  show f = (description f) ++ ": " ++ val ++ "\t\t\tbytes: " ++ show start ++ " - " ++ show end ++ getLocSizeStr end start 
           where 
             val = case value f of
                     Just x -> show x
                     Nothing -> "not set"
             start = startByte f
             end = endByte f

getSegyBytes :: Int -> Get Int
getSegyBytes x = case x of 
      1 -> getWord16be >>= return . fromIntegral
      3 -> getWord32be >>= return . fromIntegral
      _ -> skip  (x + 1) >> return (-1)

getHeader :: ByteLoc -> Get ByteLoc
getHeader f = do
  d <- getSegyBytes $ endByte f - startByte f
  return $ f { value = Just d } 

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
    val <- forM [1 .. numSamples] $ \func -> do getWord32be
    return $ val

getTrace :: Int -> Int -> Get Trace
getTrace numSamples sampleFormat = do
    th <- T.mapM getHeader defaultTraceHeader
    samples <- getTraceData numSamples 
    case sampleFormat of 
      5 -> return $ (Trace th $ wordToFloat <$> samples)
      1 -> return $ (Trace th $ wordToFloat . ibmToIeee754 <$> samples)
      _ -> error "Error: only ibm floating poins or ieee754 sample formats are supported."

getAllTraces :: Int -> Int -> Get [Trace]
getAllTraces n f = do
    empty <- isEmpty
    if empty
      then return []
    else do
      t <- getTrace n f
      rest <- getAllTraces n f
      return (t:rest)

getSEGY :: Get Output  
getSEGY = do
    h <- getTextHeader
    b <- T.mapM getHeader defaultBinaryHeader
    let n = fromJust $ value (numSamplesTrace b) 
    let f = fromJust $ value (sampleFormat b)
    t <- getAllTraces n f 
    return $ Output h b t 

readSegyLazy :: FilePath -> IO BL.ByteString
readSegyLazy file = do
    handle <- openFile file ReadMode
    BL.hGetContents handle

printEbcdic :: Output -> IO ()
printEbcdic output = BC.putStrLn $ BC.unlines (ebcdic output)

printBinaryHeader :: Output -> IO ()
printBinaryHeader output = T.mapM print (binaryHeader output) >> return ()

printOneTrace :: Trace -> IO ()
printOneTrace trace = do
    let vec = dataPoints trace
    let vecMin = show $ minimum vec
    let vecMax = show $ maximum vec
    putStrLn $ "\nTrace summary, min/max " ++ vecMin ++ "/" ++ vecMax ++ " m/s"
    putStrLn . Pr.ppShow $ take 10 vec
    T.mapM print (traceHeader trace) >> return ()

printTraces :: Int -> Output -> IO()
printTraces n output = do
    let t = take n $ traces output
    mapM_ printOneTrace t

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

--parseFile opts output = printBinaryHeader output
--  when (optPrintEbcdic opts) $ printEbcdic output
  --when (optPrintBinary opts) $ 
  --when (isJust $ optPrintTraces opts) $ printTraces num output
  --  where num = read (fromJust $ optPrintTraces opts) :: Int

printSummary :: Output -> IO ()
printSummary output = do
    putStrLn "\nSummary:"
    putStrLn $ "Parsed "++ len ++ " traces"
  where 
    len = show $ length (traces output)

--printTrace' :: Monad m => (a -> m b) -> BinaryHeader -> BL.ByteString -> m ()
--printTrace' func b stream = do
--    let n = fromJust $ value (numSamplesTrace b) 
--    let f = fromJust $ value (sampleFormat b)
--    t <- getTrace n f
--    func t




main :: IO()
main = do
  args <- getArgs
  (opts, strs) <- compilerOpts args
  when (null strs) $ error header

  streams <- mapM readSegyLazy strs
  let (x:xs) = runGet getSEGY <$> streams

  printEbcdic x
  printBinaryHeader x 
  printTraces 2  x
  printSummary x


  --putStrLn . Pr.ppShow $ strs
  --putStrLn . Pr.ppShow $ opts
