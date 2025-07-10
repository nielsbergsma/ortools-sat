module OrTools.Sat.Model.Parameters
  ( Parameters(..)
  , defaultParameters
  ) where

import Data.Int (Int32)
import qualified OrTools.Sat.Model.Encode as Encode

-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/sat_parameters.proto
data Parameters = Parameters
  { parametersNumberOfWorkers :: Int32
  , parametersMaxTimeInSeconds :: Double
  , parametersEnumerateAllSolutions :: Bool
  , parametersFillAdditionalSolutions :: Bool
  }


defaultParameters :: Parameters
defaultParameters = Parameters
  { parametersNumberOfWorkers = 1
  , parametersMaxTimeInSeconds = 10.0
  , parametersEnumerateAllSolutions = False
  , parametersFillAdditionalSolutions = False
  }


instance Encode.Encodable Parameters where
  build Parameters{..} = 
    mconcat
      [ Encode.double 36 parametersMaxTimeInSeconds
      , Encode.bool 87 parametersEnumerateAllSolutions
      , Encode.int32 100 parametersNumberOfWorkers
      , Encode.bool 194 parametersFillAdditionalSolutions 
      ]
