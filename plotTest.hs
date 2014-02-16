import Graphics.Rendering.Cairo (Render)
import Graphics.Rendering.Plot
import Numeric.GSL
import Numeric.GSL.Statistics
import Numeric.LinearAlgebra

ln = 25
ts = linspace ln (0,1)
rs = randomVector 0 Gaussian ln
 
ss = sin (15*2*pi*ts)
ds = 0.25*rs + ss
 

test_graph :: Figure ()
test_graph = do
       withTextDefaults $ setFontFamily "OpenSymbol"
       withTitle $ setText "Testing plot package:"
       setPlots 1 1
       withPlot (1,1) $ do
                        setDataset (ts,[line ts blue])
                        addAxis XAxis (Side Lower) $ withAxisLabel $ setText "time (s)"
                        addAxis YAxis (Side Lower) $ withAxisLabel $ setText "amplitude"
                        addAxis XAxis (Value 0) $ return ()
                        setRangeFromData XAxis Lower Linear
                        setRange YAxis Lower Linear (-1.25) 1.25


main = do
  writeFigure PNG "foo.png" (400, 400) test_graph 
  print ts


