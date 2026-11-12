module Main (main) where
import PiecewiseMatrixMultiplication
import qualified Data.Vector as V
-- plus your Layers module and piecewiseMatrixMult / pieceFunc
import Graphics.Gnuplot.Simple
import CollatzExample


main :: IO ()
main = do
  let numberPath = pieceFuncPath collatzMatrixs (V.fromList [9, 2, 0])
  print numberPath
  plotPathStyle
    [Title "My points", XLabel "x", YLabel "y"
    , PNG "points.png"
    , Custom "key" ["off"]]
    (defaultStyle {plotType = LinesPoints})
    [(1, 2), (2,4), (3, 16 :: Double)]
  
            


