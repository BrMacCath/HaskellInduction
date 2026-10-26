-- Create a function that will take a list of matrices and
-- Produce a piecewise function

-- What does it take
-- List of matrices.
-- Each matrix satisfies that it is a column sum matrix.

module PiecewiseMatrixMultiplication(
    piecewiseMatrixMult
) where

import Control.Monad (guard)
import Data.Matrix
import Data.Ratio (numerator, denominator)
import qualified Data.Vector as V

type Vec = V.Vector Rational

type Layer = (Matrix Rational, Vec)

-- Int is the common row count n
data Layers = Layers Int [(Matrix Rational, Vec)]

mkLayers :: [(Matrix Rational, Vec)] -> Maybe Layers
mkLayers []              = Nothing          -- n is undefined for an empty list
mkLayers ps@((m0, _) : _) =
  let n = nrows m0
      ok (m, v) = nrows m == n && V.length v == n
  in if all ok ps then Just (Layers n ps) else Nothing


layersDim :: Layers -> Int
layersDim (Layers n _) = n

getLayers :: Layers -> [(Matrix Rational, Vec)]
getLayers (Layers _ ps) = ps

piecewiseMatrixMult :: [(Matrix Rational, Vec)] -> Maybe (Vec -> Maybe Vec)
piecewiseMatrixMult matVecList = do
  layers <- mkLayers matVecList
  return (pieceFunc layers)

pieceFunc :: Layers -> Vec -> Maybe Vec
pieceFunc layers x = do
  let s = V.sum x
  guard (denominator s == 1)                   -- sum must be a whole number
  guard (V.length x == layersDim layers)       -- input must have length n

  let ps    = getLayers layers
      index = fromIntegral (numerator s `mod` fromIntegral (length ps)) :: Int
      (m, v) = ps !! index                     -- always in range thanks to mod

  return (getCol 1 (m * colVector x + colVector v))