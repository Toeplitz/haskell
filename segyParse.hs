{-# OPTIONS -XBangPatterns -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

import Codec.Text.IConv (convert)
import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Binary.Get as G
import Data.Binary.IEEE754 (wordToFloat)
import Data.List
import Data.List.Split
import Data.Int (Int32)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Word (Word32)
import System.Environment
import System.Console.GetOpt
import System.IO
import System.Posix.Files 

import qualified Control.Foldl as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Text.Show.Pretty as Pr

import GHC.Float
import Graphics.Rendering.Cairo (Render)
import Graphics.Rendering.Plot
import Numeric.GSL
import Numeric.GSL.Statistics
import Numeric.LinearAlgebra
import Data.Packed.Matrix


data TraceOut = TraceOut
  { traceSamples :: [Float]
  , inline       :: Int
  , xline        :: Int
  } deriving Show

data TraceStats = TraceStats 
  { traceMin :: Maybe Float
  , traceMax :: Maybe Float
  } deriving Show


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
  } 

getTextHeaderLine = convert "IBM1047" "UTF8" <$> G.getLazyByteString 80
getTextFileHeader = G.getLazyByteString 3200

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

getSegyBytes :: Int -> G.Get Int
getSegyBytes x = case x of 
      1 -> G.getWord16be >>= return . fromIntegral
      3 -> G.getWord32be >>= return . fromIntegral
      _ -> G.skip  (x + 1) >> return (-1)

getHeader :: ByteLoc -> G.Get ByteLoc
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

getTraceData :: Int -> G.Get [Word32]
getTraceData numSamples = do
    val <- forM [1 .. numSamples] $ \func -> do G.getWord32be
    return $ val

getSEGY :: G.Get Output  
getSEGY = do
    h <- getTextHeader
    b <- T.mapM getHeader defaultBinaryHeader
    return $ Output h b

printEbcdic :: Output -> IO ()
printEbcdic output = BC.putStrLn $ BC.unlines (ebcdic output)

printBinaryHeader :: Output -> IO ()
printBinaryHeader output = T.mapM print (binaryHeader output) >> return ()

printOneTrace :: Trace -> IO ()
printOneTrace trace = do
    let vec = dataPoints trace
    let stats = L.fold getTraceStats $ dataPoints trace
    putStrLn . Pr.ppShow $ take 10 vec
    putStrLn $ "min/max: " ++ show (traceMin stats) ++ "/" ++ show (traceMax stats) ++ "\n"
    T.mapM print (traceHeader trace) >> return ()

printTraces :: Int -> [Trace] -> IO()
printTraces n traces = do
   let t = take n traces 
   mapM_ printOneTrace t

data Options = Options
 { optShowVersion     :: Bool
 , optPrintEbcdic     :: Bool
 , optPrintBinary     :: Bool
 , optPrintSummary    :: Bool
 , optPrintTrcSummary :: Bool
 , optPlotInline      :: Maybe String
 , optPrintTraces     :: Maybe String
 } deriving Show

defaultOptions        = Options
 { optShowVersion     = False
 , optPrintEbcdic     = False
 , optPrintBinary     = False
 , optPrintSummary    = False
 , optPrintTrcSummary = False
 , optPlotInline      = Nothing
 , optPrintTraces     = Nothing
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v'] ["version"]
     (NoArg (\opts -> opts { optShowVersion = True }))
     "show version number"
 , Option ['b'] ["binary"]
     (NoArg (\opts -> opts { optPrintBinary = True }))
     "print binary header"
 , Option ['e'] ["ebcdic"]
     (NoArg (\opts -> opts { optPrintEbcdic = True }))
     "print ebcdic header"
 , Option ['f'] [""]
     (NoArg (\opts -> opts { optPrintTrcSummary = True }))
     "scan through entire file and print trace data summary"
 , Option ['p'] ["plot"]
     (OptArg ((\f opts -> opts { optPlotInline = Just f }) . fromMaybe "plot") "N")
       "plot grayscale png image of inline N to [filename]"
 , Option ['s'] ["summary"]
     (NoArg (\opts -> opts { optPrintSummary = True }))
     "scan through entire file and print trace header summary"
 , Option ['t'] ["trace"]
     (OptArg ((\f opts -> opts { optPrintTraces = Just f }) . fromMaybe "trace") "N")
       "print formatted data from the N first traces"
 ]

header :: String
header =  "Usage: segyParse.hs [OPTION...] files..."

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))


printSummary traceout = do
  let (ilMin, ilMax) = getExtrema inline traceout
  let (xlMin, xlMax) = getExtrema xline traceout

  putStrLn $ "Min/max inlines: " ++ show ilMin ++ "/" ++ show ilMax
  putStrLn $ "Min/max xlines: " ++ show xlMin ++ "/" ++ show xlMax

getSamples :: Int -> Int -> G.Get [Float]
getSamples numSamples sampleFormat = do
    samples <- getTraceData numSamples 
    case sampleFormat of 
      5 -> return $ (wordToFloat <$> samples)
      1 -> return $ (wordToFloat . ibmToIeee754 <$> samples)
      _ -> error "Error: only ibm floating poins or ieee754 sample formats are supported."

getTraceOut :: Int -> Int -> G.Get TraceOut
getTraceOut numSamples sampleFormat = do
    G.skip 4
    il <- getSegyBytes 3
    G.skip 12
    xl <- getSegyBytes 3
    G.skip (240 - 24)
    samples <- getSamples numSamples sampleFormat
    return $ TraceOut samples il xl

getTraceOutHeaders :: Int -> Int -> G.Get TraceOut
getTraceOutHeaders numSamples sampleFormat = do
    G.skip 4
    il <- getSegyBytes 3
    G.skip 12
    xl <- getSegyBytes 3
    G.skip (240 - 24)
    G.skip (numSamples * 4)
    return $ TraceOut [] il xl

getSamplesOnly :: Int -> Int -> G.Get [Float]
getSamplesOnly numSamples sampleFormat = do
    G.skip 240
    getSamples numSamples sampleFormat

getTrace :: Int -> Int -> G.Get Trace
getTrace numSamples sampleFormat = do
    th <- T.mapM getHeader defaultTraceHeader
    samples <- getSamples numSamples sampleFormat
    return $ Trace th samples

readSegyLazy :: FilePath -> IO BL.ByteString
readSegyLazy file = do
    handle <- openFile file ReadMode
    BL.hGetContents handle

getFromSegy :: G.Get a -> BLI.ByteString -> [a]
getFromSegy f input0 = go decoder input0
  where
    decoder = G.runGetIncremental f

    go (G.Done leftover _consumed trace) input =
      trace : go decoder (BLI.chunk leftover input)
    go (G.Partial k) input                     =
      go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (G.Fail _leftover _consumed msg) _input =
      []

takeHeadChunk :: BLI.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs =
    case lbs of
      (BLI.Chunk bs _) -> Just bs
      _ -> Nothing

dropHeadChunk :: BLI.ByteString -> BLI.ByteString
dropHeadChunk lbs =
    case lbs of
      (BLI.Chunk _ lbs') -> lbs'
      _ -> BLI.Empty

getTraceStats :: L.Fold Float TraceStats
getTraceStats = TraceStats <$> L.minimum <*> L.maximum

printGlobalTraceStats :: [[Float]] -> IO ()
printGlobalTraceStats x = print $ L.fold getTraceStats (concat x)

getExtrema :: (Functor f, Ord a, F.Foldable f) => (a1 -> a) -> f a1 -> (Maybe a, Maybe a)
getExtrema member s = L.fold ((,) <$> L.minimum <*> L.maximum) (member <$> s)

--segyActions :: BLI.ByteString -> Output -> IO ()
--segyActions rest x = do
--  let dt = fromIntegral (fromJust $ value (sampleInterval $ binaryHeader x)) :: Float
--  let y = (\x -> x * dt / 1000) <$> [0 .. fromIntegral n :: Float]
--  let foo = traceSamples (head $ filter (\x -> inline x == 200 && xline x == 10) traceout)
--  let bar = fmap float2Double foo
--  let bary = fmap float2Double y
--  let baz = fromList bar :: Vector Double
--  let bazy = fromList bary :: Vector Double
--  writeFigure SVG "foo.svg" (640, 640) (createGraphFigure baz bazy)
--  return ()

plotLineExec x filename traceout = do
  putStrLn $ "Parsing inline: " ++ show x ++ " and writing file: " ++ filename
  let selected = filter (\f -> inline f == x) traceout
  let values = float2Double <$> concat (traceSamples <$> selected)
  let n = length $ traceSamples (head selected)

  let mat = trans $ (length selected><n)(values)
  writeFigure SVG filename (1200, 800) (createMatrixFigure mat)
  s <- getFileStatus filename 
  let fsize = (fromIntegral $ fileSize s) :: Float
  putStrLn $ "Wrote " ++ show (fsize / 1024) ++ " kb to '" ++ filename ++ "'"

plotLine opt traceout = do
    case (findIndex (\f -> f == ',') opt) of
      Nothing -> error "argument parsing failed!"
      Just a -> plotLineExec (read x :: Int) filename traceout
        where 
          [x, filename] = splitOn "," opt

getSampleData :: Output -> (Int, Int)
getSampleData x = (a, b)
  where 
    a = fromJust $ value (numSamplesTrace $ binaryHeader x) 
    b = fromJust $ value (sampleFormat $ binaryHeader x)


segyActions' :: Options -> BLI.ByteString -> Output -> IO ()
segyActions' opts rest output = do
  when (optPrintEbcdic opts)          $ printEbcdic output
  when (optPrintBinary opts)          $ printBinaryHeader output
  when (optPrintSummary opts)         $ printSummary traceouthdrs
  when (optPrintTrcSummary opts)      $ printGlobalTraceStats samples
  when (isJust $ optPlotInline opts)  $ plotLine str traceout
  when (isJust $ optPrintTraces opts) $ printTraces num traces
    where 
      (n, f) = getSampleData output
      traceouthdrs = getFromSegy (getTraceOutHeaders n f) rest
      traceout     = getFromSegy (getTraceOut n f) rest
      samples      = getFromSegy (getSamplesOnly n f) rest
      traces       = getFromSegy (getTrace n f) rest
      num          = read (fromJust $ optPrintTraces opts) :: Int
      str          = fromJust $ optPlotInline opts

createMatrixFigure m = do
  setPlots 1 1
  withPlot (1,1) $ do 
    setDataset m
    addAxis XAxis (Side Upper) $ withAxisLabel $ setText "Tace"
    addAxis YAxis (Side Lower) $ withAxisLabel $ setText "Depth/Time"
    setRangeFromData XAxis Lower Linear
    setRangeFromData YAxis Lower Linear

createGraphFigure x y = do
 withTextDefaults $ setFontFamily "OpenSymbol"
 withTitle $ setText "Testing plot package:"
 setPlots 1 1
 withPlot (1,1) $ do
    setDataset (x,[line y blue, point y black])
    addAxis XAxis (Side Lower) $ withAxisLabel $ setText "time (s)"
    addAxis YAxis (Side Lower) $ withAxisLabel $ setText "amplitude"
    addAxis XAxis (Value 0) $ return ()
    setRangeFromData XAxis Lower Linear
    setRangeFromData YAxis Lower Linear
    --setRange YAxis Lower Linear (-1.25) 1.25

parseFile opts stream = do
  case G.runGetOrFail getSEGY stream of 
    Left  (lbs, o, err) -> error "Read failed, exiting!"
    Right (lbs, o, res) -> segyActions' opts lbs res

main :: IO()
main = do
  args <- getArgs
  (opts, strs) <- compilerOpts args
  when (null strs) $ error header

  streams <- mapM readSegyLazy strs
  mapM_ (parseFile opts) streams
