-- This is to run tests on if it is a square matrix from 
--  its list representation

module SquareMatrix(
    checkSquareMatrix,
) where

import Data.Matrix (Matrix, fromLists)
import MatrixError

checkSquareMatrix ::  [[a]] -> Either MatrixError (Matrix a)
checkSquareMatrix [] = Left EmptyMatrix

checkSquareMatrix rows
  | any ((/=columnLength) . length) rows = Left NotSquare
  | otherwise                       = Right (fromLists rows)
  where
    columnLength = length rows