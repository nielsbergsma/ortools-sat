module OrTools.Sat.Model.Variable
  ( Variable(..)
  , VariableReference
  , variableNot
  , variableReference
  ) where

import qualified OrTools.Sat.Model.Encode as Encode
import Data.Int (Int32, Int64)


-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L45
type VariableReference = Int32

data Variable 
  = IntegerVariable VariableReference Int64 Int64
  | BooleanVariable VariableReference Bool


variableNot :: Variable -> Variable
variableNot (BooleanVariable reference negated) = BooleanVariable reference (not negated)
variableNot other = other


variableReference :: Variable -> VariableReference
variableReference (IntegerVariable reference _ _) = reference
variableReference (BooleanVariable reference _) = reference


instance Encode.Encodable Variable where
  build (IntegerVariable _ lowerBound upperBound) = 
    Encode.packedVarints 2 [fromIntegral lowerBound, fromIntegral upperBound]

  build (BooleanVariable _ _) =
    Encode.packedVarints 2 [0, 1]
