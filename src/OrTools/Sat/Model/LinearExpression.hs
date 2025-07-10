module OrTools.Sat.Model.LinearExpression
  ( LinearExpression(..)
  , ToLinearExpression(..)
  ) where

import Data.Int (Int64)

import OrTools.Sat.Model.Variable
import OrTools.Sat.Model.Constant

data LinearExpression = LinearExpression
  { linearExpressionVariables :: [Variable]
  , linearExpressionCoefficients :: [Int64]
  , linearExpressionConstant :: Int64
  }

instance Semigroup LinearExpression where
  left <> right = 
    LinearExpression
      { linearExpressionVariables = left.linearExpressionVariables <> right.linearExpressionVariables
      , linearExpressionCoefficients = left.linearExpressionCoefficients <> right.linearExpressionCoefficients
      , linearExpressionConstant = left.linearExpressionConstant + right.linearExpressionConstant
      }


instance Monoid LinearExpression where
  mempty = LinearExpression [] [] 0


class ToLinearExpression a where
  toLinearExpression :: a -> LinearExpression


-- instances
instance ToLinearExpression LinearExpression where
  toLinearExpression expression = expression

instance ToLinearExpression Variable where
  toLinearExpression variable@(BooleanVariable _ True) =
    LinearExpression 
      { linearExpressionVariables = [variable]
      , linearExpressionCoefficients = [-1]
      , linearExpressionConstant = 1
      }

  toLinearExpression variable = 
    LinearExpression 
      { linearExpressionVariables = [variable]
      , linearExpressionCoefficients = [1]
      , linearExpressionConstant = 0
      }


instance ToLinearExpression Constant where
  toLinearExpression (Constant value) = 
    LinearExpression 
      { linearExpressionVariables = mempty
      , linearExpressionCoefficients = mempty
      , linearExpressionConstant = value
      }