import Data.List
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.Binary.IEEE754 as I
import Control.Monad

putRawDoubles :: P.Put
putRawDoubles = do
    let xs = [0.1]
    P.putWord64le $ genericLength xs
    mapM_ I.putFloat64le xs

getRawDoubles :: G.Get Double
getRawDoubles = I.getFloat64le

main :: IO ()
main = do
    let file = "test.bin"
    let p = P.runPut putRawDoubles 
    BL.writeFile file p

    content <- BL.readFile file
    let g = G.runGet getRawDoubles content
    print $ show g
