module OrTools.Sat.Model.Model
  ( Model(..)
  , emptyModel
  ) where

import qualified OrTools.Sat.Model.Encode as Encode

import OrTools.Sat.Model.Variable
import OrTools.Sat.Model.Constraint
import OrTools.Sat.Model.Objective


-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L636
data Model = Model
  { modelVariables :: [Variable]
  , modelConstraints :: [Constraint]
  , modelObjective :: Maybe Objective
  }


emptyModel :: Model
emptyModel = Model mempty mempty Nothing

instance Encode.Encodable Model where
  build Model{..} = 
    mconcat
      [ mconcat (Encode.embedded 2 . Encode.build <$> modelVariables)
      , mconcat (Encode.embedded 3 . Encode.build <$> modelConstraints)
      , Encode.optional (Encode.embedded 4 . Encode.build) modelObjective
      ]

