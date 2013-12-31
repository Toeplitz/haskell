import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- Latin 1 / Open Systems (US 3270)
-- http://publib.boulder.ibm.com/infocenter/tivihelp/v24r1/index.jsp?topic=%2Fcom.ibm.itcama.doc_6.2.3%2Fitcam_oraclerac63200.htm

main = do
    print $ C.converterNames
    print $ C.standardNames
    conv <- C.open "ibm-1047_P100-1995" Nothing
    contents <- B.readFile "vel_z6.25m_x12.5m_exact.segy"
    print $ C.toUnicode conv (B.take 3200 contents)

