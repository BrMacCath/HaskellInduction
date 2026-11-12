module SquareMatrixSpec.SquareMatrixSpec (spec) where

import SquareMatrix
import Test.Hspec
import MatrixError
import Data.Matrix

spec :: Spec

spec = describe "Handles check square matrix" $ do
  it "Accepts Square Matrixs" $
    checkSquareMatrix [[1,2],[3,4]] `shouldBe` Right (fromLists [[1,2],[3,4]])
  it "Rejects non square matrix" $
    checkSquareMatrix [[1,2],[3]] `shouldBe` Left NotSquare
