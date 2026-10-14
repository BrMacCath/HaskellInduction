-- A column sum matrix is a matrix where the sum 
-- of all the columns are the same number. 
-- This package is to take a matrix and ensure that
-- it is actually a column matrix.


module ColumnSumMatrix(
    createColumnSumMatrix,
    ColumnMatrixError(..)
) where

import Data.List (transpose)
import Data.Matrix (Matrix, fromLists)
import Data.Ratio ((%))

data ColumnMatrixError
  = EmptyMatrix            
  | EmptyRows              
  | RaggedRows             
  | ColumnsDoNotSumToSameNumber   
  deriving (Show, Eq)



-- Here I want a function that creates a column sum matrix
-- and returns an error if there is none. The user needs to state
-- their entries.


createColumnSumMatrix ::  [[Rational]] -> Either ColumnMatrixError (Matrix Rational)
createColumnSumMatrix [] = Left EmptyMatrix

createColumnSumMatrix rows@(r : rs)
  | null r                                   = Left EmptyRows
  | any ((/= length r) . length) rs          = Left RaggedRows
  | any ((/= target) . sum) (transpose rows) = Left ColumnsDoNotSumToSameNumber
  | otherwise                                = Right (fromLists rows)
  where
    colSums = map sum (transpose rows)
    target  = sum (map head rows)
