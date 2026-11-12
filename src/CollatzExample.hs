module CollatzExample where
    
import qualified Data.Vector as V
import Data.Matrix
import Data.Ratio ((%))
import VectorType


collatzMatrixs :: [(Matrix Rational, Vec)]
collatzMatrixs =
  [   
   (fromLists [[1%2,0,0],[0,1%2,0],[0,0,1%2]], V.fromList [0 ,0, 0]) 
    ,(fromLists [[3%1
  , 0,0 ], [0,3
  , 0 ],[0,0,3 ]], V.fromList [0,0,1])
  ]
