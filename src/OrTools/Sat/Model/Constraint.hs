module OrTools.Sat.Model.Constraint
  ( Constraint(..)
  ) where

import qualified OrTools.Sat.Model.Encode as Encode

import OrTools.Sat.Model.Variable
import OrTools.Sat.Model.LinearExpression


-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L319
data Constraint
  = AllDifferent [LinearExpression]
  | Equality LinearExpression LinearExpression
  | NotEqual LinearExpression LinearExpression
  | MinEquality LinearExpression [LinearExpression]
  | MaxEquality LinearExpression [LinearExpression]
  | LessOrEqual LinearExpression LinearExpression
  | Enforcement Variable Constraint


instance Encode.Encodable Constraint where
  build (AllDifferent expressions) =
    Encode.embedded 13 $ mconcat (encodeLinearExpression <$> expressions)
      where
        encodeLinearExpression LinearExpression{..} =
          Encode.embedded 1 $ mconcat
            [ Encode.packedVarints 1 (fromIntegral . variableReference <$> linearExpressionVariables)
            , Encode.packedVarints 2 (fromIntegral <$> linearExpressionCoefficients)
            ]

  build (Equality left right) =
    Encode.embedded 12 $ mconcat
      [ Encode.packedVarints 1 (fromIntegral . variableReference <$> variables)
      , Encode.packedVarints 2 (fromIntegral <$> coefficients)
      , Encode.packedVarints 3 (fromIntegral <$> [domain, domain])
      ]
      where 
        variables = left.linearExpressionVariables <> right.linearExpressionVariables
        coefficients = left.linearExpressionCoefficients <> (negate <$> right.linearExpressionCoefficients)
        domain = right.linearExpressionConstant - left.linearExpressionConstant

  build (NotEqual left right) =
    Encode.embedded 12 $ mconcat
        [ Encode.packedVarints 1 (fromIntegral . variableReference <$> variables)
        , Encode.packedVarints 2 (fromIntegral <$> coefficients)
        , Encode.packedVarints 3 (fromIntegral <$> [minBound, domain - 1, domain + 1, maxBound])
        ]
      where 
        variables = left.linearExpressionVariables <> right.linearExpressionVariables
        coefficients = left.linearExpressionCoefficients <> (negate <$> right.linearExpressionCoefficients)
        domain = right.linearExpressionConstant - left.linearExpressionConstant

  build (MinEquality target expressions) =
    Encode.embedded 27 $ mconcat
      [ Encode.embedded 1 . encodeNegativeLinearExpression $ target
      , mconcat (Encode.embedded 2 . encodeNegativeLinearExpression <$> expressions)
      ]
      where 
        encodeNegativeLinearExpression LinearExpression{..} =
          mconcat
            [ Encode.packedVarints 1 (fromIntegral . variableReference <$> linearExpressionVariables)
            , Encode.packedVarints 2 (fromIntegral . negate <$> linearExpressionCoefficients)
            , Encode.nonZeroInt64 3 (negate linearExpressionConstant)
            ]

  build (MaxEquality target expressions) =
    Encode.embedded 27 $ mconcat
      [ Encode.embedded 1 . encodeNegativeLinearExpression $ target
      , mconcat (Encode.embedded 2 . encodeNegativeLinearExpression <$> expressions)
      ]
      where 
        encodeNegativeLinearExpression LinearExpression{..} =
          mconcat
            [ Encode.packedVarints 1 (fromIntegral . variableReference <$> linearExpressionVariables)
            , Encode.packedVarints 2 (fromIntegral <$> linearExpressionCoefficients)
            , Encode.nonZeroInt64 3 linearExpressionConstant
            ]

  build (LessOrEqual left right) =
    Encode.embedded 12 $ mconcat
      [ Encode.packedVarints 1 (fromIntegral . variableReference <$> variables)
      , Encode.packedVarints 2 (fromIntegral <$> coefficients)
      , Encode.packedVarints 3 (fromIntegral <$> [minBound, domain])
      ]
      where 
        variables = left.linearExpressionVariables <> right.linearExpressionVariables
        coefficients = left.linearExpressionCoefficients <> (negate <$> right.linearExpressionCoefficients)
        domain = right.linearExpressionConstant - left.linearExpressionConstant

  build (Enforcement (BooleanVariable reference negated) constraint) = 
    mconcat
      [ Encode.packedVarints 2 [fromIntegral (literal negated)]
      , Encode.build constraint
      ]
    where
      literal True = (-1 - reference)
      literal False = reference

  build (Enforcement _ constraint) = 
    Encode.build constraint
