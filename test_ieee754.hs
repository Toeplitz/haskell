import Data.List
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Binary.IEEE754 as I
import Control.Monad


putDoubles :: [Double] -> Put
putDoubles xs = do
    mapM_ I.putFloat64le xs


getListOfDoubles :: Get [Double]
getListOfDoubles = do
    empty <- isEmpty
    if empty
      then return []
      else do
        v <- I.getFloat64le
        rest <- getListOfDoubles
        return (v:rest)


getDouble :: Get Double
getDouble = I.getFloat64le


main :: IO ()
main = do
    let xs = [0.1, 0.2, pi]
    let file = "test.bin"

    let p = runPut $ putDoubles xs
    BL.writeFile file p

    content <- BL.readFile file
    let g = runGet getListOfDoubles content
    print $ g
    print $ length g
