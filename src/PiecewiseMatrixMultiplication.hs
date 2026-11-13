-- Create a function that will take a list of matrices and
-- Produce a piecewise function

-- What does it take
-- List of matrices.
-- Each matrix satisfies that it is a column sum matrix.

module PiecewiseMatrixMultiplication(
    piecewiseMatrixMult,
    pieceFuncPath,
    pieceFuncPathList
) where

import Control.Monad (guard)
import Data.Matrix
import Data.Ratio (numerator, denominator)
import qualified Data.Vector as V

type Vec = V.Vector Rational

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


-- This is a piecewise function on whole numbers
pieceFunc :: Layers -> Vec -> Maybe Vec
pieceFunc layers x = do
  let s = V.sum x
  guard (denominator s == 1)                   -- sum must be a whole number
  guard (V.length x == layersDim layers)       -- input must have length n

  let ps    = getLayers layers
      index = fromIntegral (numerator s `mod` fromIntegral (length ps)) :: Int
      (m, v) = ps !! index                     -- always in range thanks to mod

  return (getCol 1 (m * colVector x + colVector v))

pieceFuncPathLoop :: (Vec -> Maybe Vec) -> Vec -> Int -> Int-> [Vec] -> [Vec]
pieceFuncPathLoop f vec current orig vecList
  | current < orig = vecList
  | otherwise =
      case f vec of
        Nothing -> vecList
        Just vec' -> 
          let current' = floor (V.sum vec') :: Int
          in pieceFuncPathLoop f vec' current' orig (vecList ++ [vec'])

createPath:: (Vec -> Maybe Vec) -> Vec -> [Vec]
createPath f origVector = 
    let origSum = floor (V.sum origVector) :: Int
    in pieceFuncPathLoop f origVector origSum origSum [origVector]

pieceFuncPath:: [(Matrix Rational, Vec)] -> Vec ->  [Vec]
pieceFuncPath matVecList origVector = do
  case piecewiseMatrixMult matVecList of
    Nothing -> []
    Just f -> createPath f origVector


pieceFuncPathList:: [(Matrix Rational, Vec)] -> [Vec] -> [[Vec] ]
pieceFuncPathList _ [] = []
pieceFuncPathList matVecList vecList = do
  case piecewiseMatrixMult matVecList of
    Nothing -> []
    Just f -> map (createPath f) vecList 