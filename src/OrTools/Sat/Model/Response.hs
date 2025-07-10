module OrTools.Sat.Model.Response
  ( Response(..)
  , Status(..)
  , parseResponse
  ) where

import qualified OrTools.Sat.Model.Decode as Decode
import qualified Data.ByteString as BS

import OrTools.Sat.Model.Solution

-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L710
data Status 
  = Unknown
  | InvalidModel
  | Feasible
  | Infeasible
  | Optimal
  deriving (Show, Eq)


decodeStatus :: Decode.Parser Decode.RawPrimitive Status
decodeStatus = do
  value <- Decode.int32
  case value of
    1 -> return InvalidModel
    2 -> return Feasible
    3 -> return Infeasible
    4 -> return Optimal
    _ -> return Unknown


-- source: https://github.com/google/or-tools/blob/stable/ortools/sat/cp_model.proto#L747C1-L761C30
data Response = Response
  { responseStatus :: Status
  , responseSolution :: Solution
  , responseObjectiveValue :: Double
  , responseAlternativeSolutions :: [Solution]
  } deriving (Show, Eq)


decodeResponse :: Decode.Parser Decode.RawMessage Response
decodeResponse = do
  responseStatus <- Decode.at (Decode.one decodeStatus Unknown) 1
  responseSolution <- Decode.at (Decode.one decodeSolution emptySolution) 2
  responseObjectiveValue <- Decode.at (Decode.one Decode.double 0) 3
  responseAlternativeSolutions <- Decode.at alternativeSolutions 27
  return Response{..}

  where
    alternativeSolutions = Decode.repeated (Decode.embedded' alternativeSolution)
    alternativeSolution = Decode.at (Decode.one decodeSolution emptySolution) 1


parseResponse :: BS.ByteString -> Either String Response
parseResponse = showLeft . Decode.parse decodeResponse
  where
    showLeft = either (Left . show) Right