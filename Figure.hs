
module Figure where

import Graphics.Rendering.Plot


makePlot :: Figure () -> FilePath -> IO ()
makePlot fig filename =  writeFigure SVG filename (1200, 800) fig


createMatrixFigure :: Dataset a => a -> Figure ()
createMatrixFigure m = do
  setPlots 1 1
  withPlot (1,1) $ do 
    setDataset m
    addAxis XAxis (Side Upper) $ withAxisLabel $ setText "Tace"
    addAxis YAxis (Side Lower) $ withAxisLabel $ setText "Depth/Time"
    setRangeFromData XAxis Lower Linear
    setRangeFromData YAxis Lower Linear
 

--createGraphFigure x y = do
-- withTextDefaults $ setFontFamily "OpenSymbol"
-- withTitle $ setText "Testing plot package:"
-- setPlots 1 1
-- withPlot (1,1) $ do
--    setDataset (x,[line y blue, point y black])
--    addAxis XAxis (Side Lower) $ withAxisLabel $ setText "time (s)"
--    addAxis YAxis (Side Lower) $ withAxisLabel $ setText "amplitude"
--    addAxis XAxis (Value 0) $ return ()
--    setRangeFromData XAxis Lower Linear
--    setRangeFromData YAxis Lower Linear
    --setRange YAxis Lower Linear (-1.25) 1.25

