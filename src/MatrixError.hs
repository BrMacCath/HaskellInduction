module MatrixError (MatrixError(..)) where

data MatrixError
  = EmptyMatrix
  | EmptyRows
  | RaggedRows
  | ColumnsDoNotSumToSameNumber
  | NotSquare
  deriving (Show, Eq)