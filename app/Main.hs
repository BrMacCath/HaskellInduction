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



loop :: (Vec -> Maybe Vec) -> Vec -> Int -> Int-> [Vec] -> IO [Vec]
loop f vec current orig vecList
  | current < orig = return vecList
  | otherwise =
      case f vec of
        Nothing -> do
          putStrLn "The function failed on this vector"
          return vecList
        Just vec' -> do
          let current' = floor (V.sum vec') :: Int
              newList  = vecList ++ [vec']
          loop f vec' current' orig newList


main :: IO ()
main = do
  let numberPath = pieceFuncPath twoPairs (V.fromList [9, 2, 0])
  print numberPath
  plotPathStyle
    [Title "My points", XLabel "x", YLabel "y"
    , PNG "points.png"
    , Custom "key" ["off"]]
    (defaultStyle {plotType = LinesPoints})
    [(1, 2), (2,4), (3, 16 :: Double)]
  
            


