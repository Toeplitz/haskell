-- Haskell Velocity Toolkit
--
--
--
import System.Environment
import Control.Monad

import Segy

main :: IO()
main = do
  args <- getArgs
  (opts, strs) <- compilerOpts args
  when (null strs) $ error header

  streams <- mapM readSegyLazy strs
  mapM_ (parseFile opts) streams
