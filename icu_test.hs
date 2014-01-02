import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


-- Segy standard:
-- http://www.seg.org/documents/10161/77915/seg_y_rev1.pdf
--
-- EBCDIC conversion usig text-icu:
-- Latin 1 / Open Systems (US 3270)
-- http://publib.boulder.ibm.com/infocenter/tivihelp/v24r1/index.jsp?topic=%2Fcom.ibm.itcama.doc_6.2.3%2Fitcam_oraclerac63200.htm


-- Example segy files:
-- http://utam.gg.utah.edu/SeismicData/WFault3D/WD_3D.sgy (905 Mb)
-- http://utam.gg.utah.edu/SeismicData/Avenue/Avenue.sgy (356 Mb)
-- test01.segy
-- test02.segy
--

-- Requirements:
-- print text header
-- print binary header values
-- print trace numbers
-- print trace sample values by trace number
--
-- Make use of Binary.Get monad?
-- http://hackage.haskell.org/package/binary-0.7.1.0/docs/Data-Binary-Get.html


main :: IO()
main = do
    conv <- C.open "ibm-1047_P100-1995" Nothing
    contents <- B.readFile "test01.segy"
    print $ C.toUnicode conv (B.take 3200 contents)
    print $ fromIntegral (B.index contents 20)
    print $ fromIntegral (B.index contents 114)
    print $ fromIntegral (B.index contents 116)

