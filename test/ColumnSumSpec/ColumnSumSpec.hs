module ColumnSumSpec.ColumnSumSpec (spec) where


import Test.Hspec
import ColumnSumMatrix
import MatrixError


spec :: Spec
spec = describe "validateColumnSumMatrix" $ do
  it "rejects an empty list" $
    validateColumnSumMatrix [] `shouldBe` Left EmptyMatrix

  it "rejects empty rows" $
    validateColumnSumMatrix [[]] `shouldBe` Left EmptyRows

  it "rejects ragged rows" $
    validateColumnSumMatrix [[1, 0], [0]] `shouldBe` Left RaggedRows

  it "rejects columns with different sums" $
    validateColumnSumMatrix [[1, 2], [1, 3]] `shouldBe` Left ColumnsDoNotSumToSameNumber
  -- column sums are 2 and 5

  it "accepts columns that all match the first" $
    validateColumnSumMatrix [[1, 2], [3, 2]] `shouldSatisfy` either (const False) (const True)
  -- column sums are 4 and 4