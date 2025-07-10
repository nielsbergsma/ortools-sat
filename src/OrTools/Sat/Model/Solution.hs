module OrTools.Sat.Model.Solution
  ( Solution(..)
  , emptySolution
  , decodeSolution
  ) where

import Data.Int (Int64)
import qualified OrTools.Sat.Model.Decode as Decode

-- source:  https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L740
data Solution = Solution 
  { solutionValues :: [Int64]
  } deriving (Show, Eq)


emptySolution :: Solution
emptySolution = 
  Solution mempty


decodeSolution :: Decode.Parser Decode.RawPrimitive Solution
decodeSolution = 
  Solution <$> Decode.packedVarints
