module PiecewiseMatrixMultSpec.PiecewiseMatrixMultSpec (spec) where

import Test.Hspec
import CollatzExample
import PiecewiseMatrixMultiplication
import Data.Ratio ((%))
import qualified Data.Vector as V

spec:: Spec

spec = describe "Testing Piecewise function" $ do
    it "Prints correct path" $
        pieceFuncPath collatzMatrixs (V.fromList [9, 2, 0]) `shouldBe` 
        map V.fromList [[9 % 1,2 % 1,0 % 1],[27 % 1,6 % 1,1 % 1],[27 % 2,3 % 1,1 % 2],[81 % 2,9 % 1,5 % 2],[81 % 4,9 % 2,5 % 4],[81 % 8,9 % 4,5 % 8],[243 % 8,27 % 4,23 % 8],[243 % 16,27 % 8,23 % 16],[243 % 32,27 % 16,23 % 32]]