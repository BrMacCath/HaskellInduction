module Main (main) where
import Data.Matrix
import Data.Ratio ((%))
import PiecewiseMatrixMultiplication
import qualified Data.Vector as V
-- plus your Layers module and piecewiseMatrixMult / pieceFunc
import Graphics.Gnuplot.Simple
type Vec = V.Vector Rational

twoPairs :: [(Matrix Rational, Vec)]
twoPairs =
  [   
   (fromLists [[1%2,0,0],[0,1%2,0],[0,0,1%2]], V.fromList [0 ,0, 0]) 
    ,(fromLists [[3%1
  , 0,0 ], [0,3
  , 0 ],[0,0,3 ]], V.fromList [0,0,1])
  ]



loop :: (Vec -> Maybe Vec) -> Vec -> Int -> Int -> IO ()
loop f vec current orig
  | current < orig = do
      putStrLn "Stopped:"
      print vec
  | otherwise =
      case f vec of
        Nothing   -> putStrLn "The function failed on this vector"
        Just vec' -> do
          let current' = floor (V.sum vec') :: Int
          print vec'
          print current'
          loop f vec' current' orig


main :: IO ()
main = do
  case piecewiseMatrixMult twoPairs of
    Nothing -> putStrLn "Invalid list of pairs"
    Just f  -> do
        let origVec    = V.fromList [9, 2, 0] :: Vec
            origSum = floor (V.sum origVec) :: Int
        loop f origVec origSum origSum
  plotPathStyle
    [Title "My points", XLabel "x", YLabel "y"
    , PNG "points.png"
    , Custom "key" ["off"]]
    (defaultStyle {plotType = LinesPoints})
    [(1, 2), (2,4), (3, 16 :: Double)]
  
            


