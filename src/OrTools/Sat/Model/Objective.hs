module OrTools.Sat.Model.Objective
  ( Objective(..)
  , minimiseObjective
  ) where

import Data.Int (Int64)

import OrTools.Sat.Model.Variable
import OrTools.Sat.Model.LinearExpression
import qualified OrTools.Sat.Model.Encode as Encode

-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L478
data Objective = Objective
  { objectiveVariables :: [Variable]
  , objectiveCoefficients :: [Int64]
  , objectiveOffset :: Double
  , objectiveScalingFactor :: Double
  }

instance Encode.Encodable Objective where
  build Objective{..} = 
    mconcat
      [ Encode.packedVarints 1 (fromIntegral . variableReference <$> objectiveVariables)
      , Encode.nonZeroDouble 2 objectiveOffset
      , Encode.nonZeroDouble 3 objectiveScalingFactor
      , Encode.packedVarints 4 (fromIntegral <$> objectiveCoefficients)
      ]

minimiseObjective :: LinearExpression -> Objective
minimiseObjective expression = 
  Objective
    { objectiveVariables = expression.linearExpressionVariables
    , objectiveCoefficients = expression.linearExpressionCoefficients
    , objectiveOffset = fromIntegral expression.linearExpressionConstant
    , objectiveScalingFactor = 0
    }
  