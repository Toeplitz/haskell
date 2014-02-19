-- Haskell Velocity Toolkit
--
--
--
import Control.Monad
import Data.Maybe (fromMaybe, fromJust, isJust)
import System.Console.GetOpt
import System.Environment

import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy.Internal as BLI

import Segy

data Options = Options
 { optShowVersion     :: Bool
 , optPrintEbcdic     :: Bool
 , optPrintBinary     :: Bool
 , optPrintSummary    :: Bool
 , optPrintTrcSummary :: Bool
 , optPlotInline      :: Maybe String
 , optPrintTraces     :: Maybe String
 } deriving Show

defaultOptions :: Options
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

runActions :: Options -> BLI.ByteString -> Output -> IO ()
runActions opts rest output = do
  when (optPrintEbcdic opts)          $ printEbcdic output
  when (optPrintBinary opts)          $ printBinaryHeader output
  when (optPrintSummary opts)         $ printSummary traceouthdrs
  when (optPrintTrcSummary opts)      $ printGlobalTraceStats samples
  when (isJust $ optPlotInline opts)  $ plotLine str traceout
  when (isJust $ optPrintTraces opts) $ printTraces num traces
    where 
      (n, f) = getSampleData output
      traceouthdrs = getFromSegy (getTraceOutHeaders n) rest
      traceout     = getFromSegy (getTraceOut n f) rest
      samples      = getFromSegy (getSamplesOnly n f) rest
      traces       = getFromSegy (getTrace n f) rest
      num          = read (fromJust $ optPrintTraces opts) :: Int
      str          = fromJust $ optPlotInline opts


parseFile :: Options -> BLI.ByteString -> IO ()
parseFile opts stream = do
  case G.runGetOrFail getSEGY stream of 
    Left  (_, _, _) -> error "Read failed, exiting!"
    Right (lbs, _, res) -> runActions opts lbs res

main :: IO()
main = do
  args <- getArgs
  (opts, strs) <- compilerOpts args
  when (null strs) $ error header

  streams <- mapM readSegyLazy strs
  mapM_ (parseFile opts) streams
