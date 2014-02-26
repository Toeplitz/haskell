{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{- |
- Module      :  Segy
- Description :  Parse seismic SEGY files
- Copyright   :  (c) Martin Sarajaervi
- License     :  GPL
-
- Maintainer  :  martin.sarajaervi <at> gmail.com
- Stability   :  unstable 
- Portability :  portable 
-
- Parsing of SEGY files defined in the rev 1 format:
- http://www.seg.org/documents/10161/77915/seg_y_rev1.pdf
- -}

module Segy where

import Codec.Text.IConv (convert)
import Control.Applicative
--import Control.Parallel.Strategies (runEval, rpar, using, parListChunk, rdeepseq)
import Control.Monad
import Data.Bits
import Data.Binary.IEEE754 (wordToFloat)
import Data.Int (Int32)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Word (Word32)
import System.IO
import System.Posix.Files 

import qualified Control.Foldl as L
import qualified Control.Monad.Par as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Text.Show.Pretty as Pr

import GHC.Float
import Numeric.LinearAlgebra

data Trace = Trace
  { traceSamples :: [Float]
  , inline       :: Int
  , xline        :: Int
  } deriving Show

data TraceFull = TraceFull
  { traceHeader :: TraceHeader ByteLoc
  , dataPoints :: [Float]
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



data Output = Output 
  { ebcdic :: [BL.ByteString]
  , binaryHeader :: BinaryHeader ByteLoc
  } 

getTextHeaderLine :: G.Get BLI.ByteString
getTextHeaderLine = convert "IBM1047" "UTF8" <$> G.getLazyByteString 80

getTextFileHeader :: G.Get BLI.ByteString
getTextFileHeader = G.getLazyByteString 3200

getTextHeader :: G.Get [BLI.ByteString]
getTextHeader = replicateM 40 getTextHeaderLine

getLocSizeStr :: Int -> Int -> String
getLocSizeStr x y = case x - y of
                      3 -> " (Int32)"
                      1 -> " (Int16)"
                      _ -> " (unknown)"

instance Show ByteLoc where
  show f = description f ++ ": " ++ val ++ "\t\t\tbytes: " ++ show start ++ " - " ++ show end ++ getLocSizeStr end start 
           where 
             val = case value f of
                     Just x -> show x
                     Nothing -> "not set"
             start = startByte f
             end = endByte f

getSegyBytes :: Int -> G.Get Int
getSegyBytes x = case x of 
      1 -> liftM fromIntegral G.getWord16be
      3 -> liftM fromIntegral G.getWord32be
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
getTraceData numSamples = forM [1 .. numSamples] $ const G.getWord32be

getSEGY :: G.Get Output  
getSEGY = do
    h <- getTextHeader
    b <- T.mapM getHeader defaultBinaryHeader
    return $ Output h b

printEbcdic :: Output -> IO ()
printEbcdic output = BC.putStrLn $ BC.unlines (ebcdic output)

printBinaryHeader :: Output -> IO ()
printBinaryHeader output = void $ T.mapM print (binaryHeader output)

printOneTrace :: TraceFull -> IO ()
printOneTrace trace = do
    let vec = dataPoints trace
    let stats = L.fold getTraceStats $ dataPoints trace
    putStrLn . Pr.ppShow $ take 10 vec
    putStrLn $ "min/max: " ++ show (traceMin stats) ++ "/" ++ show (traceMax stats) ++ "\n"
    void $ T.mapM print (traceHeader trace)

printTraces :: Int -> [TraceFull] -> IO()
printTraces n traces = do
   let t = take n traces 
   mapM_ printOneTrace t

printSummary :: (Functor f, F.Foldable f) => f Trace -> IO ()
printSummary trace = do
  let (ilMin, ilMax) = getExtrema inline trace
  let (xlMin, xlMax) = getExtrema xline trace

  putStrLn $ "Min/max inlines: " ++ show ilMin ++ "/" ++ show ilMax
  putStrLn $ "Min/max xlines: " ++ show xlMin ++ "/" ++ show xlMax

getSamples :: Int -> Int -> G.Get [Float]
getSamples numSamples f = do
    samples <- getTraceData numSamples 
    case f of 
      5 -> return $ wordToFloat <$> samples
      1 -> return $ wordToFloat . ibmToIeee754 <$> samples
      _ -> error "Error: only ibm floating poins or ieee754 sample formats are supported."

traceBytes :: G.Get (Int, Int)
traceBytes = do
    G.skip 4
    il <- getSegyBytes 3
    G.skip 12
    xl <- getSegyBytes 3
    G.skip (240 - 24)
    return (il, xl)

getTrace :: Int -> Int -> G.Get Trace
getTrace numSamples f = do
    (il, xl) <- traceBytes
    samples <- getSamples numSamples f
    return $ Trace samples il xl

getTraceHeaders :: Int -> G.Get Trace
getTraceHeaders numSamples = do
    (il, xl) <-traceBytes
    G.skip (numSamples * 4)
    return $ Trace [] il xl

getSamplesOnly :: Int -> Int -> G.Get [Float]
getSamplesOnly numSamples f = do
    G.skip 240
    getSamples numSamples f 

getSampleData :: Output -> (Int, Int)
getSampleData x = (a, b)
  where 
    a = fromJust $ value (numSamplesTrace $ binaryHeader x) 
    b = fromJust $ value (sampleFormat $ binaryHeader x)


getTrace' :: Int -> Int -> G.Get TraceFull
getTrace' numSamples f = do
    th <- T.mapM getHeader defaultTraceHeader
    samples <- getSamples numSamples f
    return $ TraceFull th samples

readSegyLazy :: FilePath -> IO BL.ByteString
readSegyLazy file = do
    handle <- openFile file ReadMode
    BL.hGetContents handle

getFromSegy :: G.Get a -> BLI.ByteString -> [a]
getFromSegy f = go decoder
  where
    decoder = G.runGetIncremental f

    go (G.Done leftover _consumed trace) input =
      trace : go decoder (BLI.chunk leftover input)
    go (G.Partial k) input                     =
      go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (G.Fail _leftover _consumed _) _input =
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


-- From http://community.haskell.org/~simonmar/slides/CUFP.pdf
parMapChunk :: P.NFData b => Int -> (a -> b) -> [a] -> P.Par [b]
parMapChunk n f xs = concat <$> P.parMap (map f) (chunk' n xs)

chunk' :: Int -> [a] -> [[a]]
chunk' _ [] = []
chunk' n xs = as : chunk' n bs where (as, bs) = splitAt n xs

printGlobalTraceStats' :: [[Float]] -> IO ()
printGlobalTraceStats' x = do
    let n = quot (length x) 4
    let l = chunksOf n x
    forM_ l (print . length)

    --runEval $ do
    --  d <- rpar (L.fold getTraceStats (head l))
    --  print (traceMin d)

--    print $ P.runPar $ parMapChunk 1000 (L.fold getTraceStats) x
    --let stats = L.fold getTraceStats (concat x)
    --print $ P.runPar $ do
    --  i1 <- P.new 
    --  i2 <- P.new 
    --  i3 <- P.new 
    --  i4 <- P.new 
    --  P.fork $ P.put_ i1 (L.fold getTraceStats (concat $ head l))
    --  P.fork $ P.put_ i2 (L.fold getTraceStats (concat $ l !! 1))
    --  P.fork $ P.put_ i3 (L.fold getTraceStats (concat $ l !! 2))
    --  P.fork $ P.put_ i4 (L.fold getTraceStats (concat $ l !! 3))
      --P.fork $ P.put_ i2 (L.fold getTraceStats (concat x))
    --  stats' <- P.get i1
    --  stats2' <- P.get i2
    --  stats3' <- P.get i3
    --  stats4' <- P.get i4
    --  return (stats', stats2', stats3', stats4')

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

getInline :: Int -> [Trace] -> [Trace]
getInline n = filter (\f -> inline f == n)

getListOfDoubles :: [Trace] -> [Double]
getListOfDoubles x = float2Double <$> concat (traceSamples <$> x)

getMatrix :: [Trace] -> Matrix Double
getMatrix x = trans $ (length x><n)values :: Matrix Double
  where 
    values = getListOfDoubles x
    n = length $ traceSamples (head x)

printFileWritten :: FilePath -> IO ()
printFileWritten filename = do
  s <- getFileStatus filename 
  let fsize = (fromIntegral $ fileSize s) :: Float
  putStrLn $ "Wrote " ++ show (fsize / 1024) ++ " kb to '" ++ filename ++ "'"
