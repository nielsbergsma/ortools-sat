{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State

import OrTools.Sat.Model.Model
import OrTools.Sat.Model.Variable
import OrTools.Sat.Model.LinearExpression
import OrTools.Sat.Model.Constant
import OrTools.Sat.Model.Objective
import OrTools.Sat.Model.Constraint
import OrTools.Sat.Model.Parameters
import OrTools.Sat.Model.Request
import OrTools.Sat.Model.Response
import OrTools.Sat.Model.Solution
import OrTools.Sat
import qualified OrTools.Sat.Model.Encode as Encode
import qualified OrTools.Sat.Model.Decode as Decode

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

main :: IO ()
main = defaultMain tests

{-
  SUT: Haskell based OR-tools SAT library + proto3-wire library
  Every supported variable, constraint, objective, etc. should yield the same protobuf encoded message

  Expected encoded message are built with C++, using ORTools 9.14
-}

tests :: TestTree
tests = testGroup "SAT tests"
  [ testEncodeParameters
  , testEncodeVariables
  , testEncodeAllDifferentConstraint
  , testEncodeEqualityConstraint
  , testEncodeNotEqualConstraint
  , testEncodeMinEqualityConstraint
  , testEncodeMaxEqualityConstraint
  , testEncodeLessOrEqualConstraint
  , testEncodeEnforcementConstraint
  , testEncodeNegatedEnforcementConstraint
  , testEncodeMinimiseObjective
  , testEncodeRequest
  , testDecodeResponse
  , testEncodeSat
  ]

--- low-level model
-- parameters
testEncodeParameters = testCase "Encode Parameters" $ 
  let 
    parameters = Parameters
      { parametersNumberOfWorkers = 3
      , parametersMaxTimeInSeconds = 12.3
      , parametersEnumerateAllSolutions = True
      , parametersFillAdditionalSolutions = False
      }

    actual = buildBase64ByteString parameters
  in
    actual @?= "oQKamZmZmZkoQLgFAaAGA5AMAA=="


-- variables
testEncodeVariables = testCase "Encode Variables (Boolean and Integer)" $
  let
    model = Model 
      { modelVariables = 
        [ IntegerVariable 0 1 100
        , IntegerVariable 1 2 200
        , BooleanVariable 2 False
        ]
      , modelConstraints = []
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgFkEgUSAwLIARIEEgIAAQ=="


-- constraints 
testEncodeAllDifferentConstraint = testCase "Encode All Different Constraint" $
  let
    x = IntegerVariable 0 1 3
    y = IntegerVariable 1 1 3
    z = IntegerVariable 2 1 3

    model = Model 
      { modelVariables = [x, y, z]
      , modelConstraints = 
        [ AllDifferent 
          [ toLinearExpression x
          , toLinearExpression y
          , toLinearExpression z
          ]
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEDEgQSAgEDEgQSAgEDGhpqGAoGCgEAEgEBCgYKAQESAQEKBgoBAhIBAQ=="


testEncodeEqualityConstraint = testCase "Encode Equality Constraint" $
  let
    x = IntegerVariable 0 1 5
    y = IntegerVariable 1 1 5

    model = Model 
      { modelVariables = [x, y]
      , modelConstraints = 
        [ Equality (toLinearExpression x) (toLinearExpression y)
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEFEgQSAgEFGhdiFQoCAAESCwH///////////8BGgIAAA=="


testEncodeNotEqualConstraint = testCase "Encode NotEqual Constraint" $
  let
    x = IntegerVariable 0 1 3
    y = IntegerVariable 1 1 3

    model = Model 
      { modelVariables = [x, y]
      , modelConstraints = 
        [ NotEqual (toLinearExpression x) (toLinearExpression y)
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEDEgQSAgEDGjNiMQoCAAESCwH///////////8BGh6AgICAgICAgIAB////////////AQH//////////38="

testEncodeMinEqualityConstraint = testCase "Encode MinEquality Constraint" $
  let
    x = IntegerVariable 0 1 5
    y = IntegerVariable 1 1 5
    z = IntegerVariable 2 1 5

    model = Model 
      { modelVariables = [x, y, z]
      , modelConstraints = 
        [ MinEquality (toLinearExpression z) [toLinearExpression x, toLinearExpression y]
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEFEgQSAgEFEgQSAgEFGjbaATMKDwoBAhIK////////////ARIPCgEAEgr///////////8BEg8KAQESCv///////////wE="



testEncodeMaxEqualityConstraint = testCase "Encode MaxEquality Constraint" $
  let
    x = IntegerVariable 0 1 5
    y = IntegerVariable 1 1 5
    z = IntegerVariable 2 1 5

    model = Model 
      { modelVariables = [x, y, z]
      , modelConstraints = 
        [ MaxEquality (toLinearExpression z) [toLinearExpression x, toLinearExpression y]
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEFEgQSAgEFEgQSAgEFGhvaARgKBgoBAhIBARIGCgEAEgEBEgYKAQESAQE="



testEncodeLessOrEqualConstraint = testCase "Encode LessOrEqual Constraint" $
  let
    x = IntegerVariable 0 1 5
    y = IntegerVariable 1 1 5

    model = Model 
      { modelVariables = [x, y]
      , modelConstraints = 
        [ LessOrEqual (toLinearExpression x) (toLinearExpression y)
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEFEgQSAgEFGiBiHgoCAAESCwH///////////8BGguAgICAgICAgIABAA=="


testEncodeEnforcementConstraint = testCase "Encode Enforcement Constraint" $
  let
    x = IntegerVariable 0 1 5
    y = IntegerVariable 1 1 5
    z = BooleanVariable 2 False

    model = Model 
      { modelVariables = [x, y, z]
      , modelConstraints = 
        [ Enforcement z (LessOrEqual (toLinearExpression x) (toLinearExpression y))
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEFEgQSAgEFEgQSAgABGiMSAQJiHgoCAAESCwH///////////8BGguAgICAgICAgIABAA=="


testEncodeNegatedEnforcementConstraint = testCase "Encode Enforcement Constraint (Negated)" $
  let
    x = IntegerVariable 0 1 5
    y = IntegerVariable 1 1 5
    z = BooleanVariable 2 True

    model = Model 
      { modelVariables = [x, y, z]
      , modelConstraints = 
        [ Enforcement z (LessOrEqual (toLinearExpression x) (toLinearExpression y))
        ]
      , modelObjective = Nothing
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgEFEgQSAgEFEgQSAgABGiwSCv3//////////wFiHgoCAAESCwH///////////8BGguAgICAgICAgIABAA=="


-- objectives
testEncodeMinimiseObjective = testCase "Encode Minimize Objective" $
  let
    x = IntegerVariable 0 0 10
    y = IntegerVariable 1 0 10
    z = Constant 10

    sumXY = toLinearExpression x <> toLinearExpression y

    model = Model 
      { modelVariables = [x, y]
      , modelConstraints = 
        [ (LessOrEqual sumXY (toLinearExpression z))
        ]
      , modelObjective = Just (minimiseObjective sumXY)
      }

    actual = buildBase64ByteString model
  in
    actual @?= "EgQSAgAKEgQSAgAKGhdiFQoCAAESAgEBGguAgICAgICAgIABCiIICgIAASICAQE="


-- request
testEncodeRequest = testCase "Encode Request" $
  let
    model = Model 
      { modelVariables = 
        [ IntegerVariable 0 1 100
        , IntegerVariable 1 2 200
        ]
      , modelConstraints = []
      , modelObjective = Nothing
      }

    parameters = Parameters
      { parametersNumberOfWorkers = 3
      , parametersMaxTimeInSeconds = 12.3
      , parametersEnumerateAllSolutions = True
      , parametersFillAdditionalSolutions = False
      }

    request = Request 
      { requestModel = model
      , requestParameters = parameters
      }

    actual = buildBase64ByteString request
  in
    actual @?= "Cg0SBBICAWQSBRIDAsgBGhOhApqZmZmZmShAuAUBoAYDkAwA"

testDecodeResponse = testCase "Decode Response" $
  let
    expected = Right $ Response
      { responseStatus = Optimal
      , responseSolution = Solution [2, 1]
      , responseObjectiveValue = 3
      , responseAlternativeSolutions = 
          [ Solution [2, 1]
          ]
      }

    actual = parseFromBase64ByteString "CAQSAgIBGQAAAAAAAAhAIQAAAAAAAAhAUAJgAnAEebzqAfOQKU8/gQG86gHzkClPP4kBH4X6fOVYlj6iAQRtYWluwAEB2gEECgICAegBA/ABAw=="
  in
    actual @?= expected


--- api
testEncodeSat = testCase "Encode Sat" $
  let
    parameters = Parameters
      { parametersNumberOfWorkers = 1
      , parametersMaxTimeInSeconds = 10.0
      , parametersEnumerateAllSolutions = False
      , parametersFillAdditionalSolutions = False
      }

    program = do
      x <- newIntegerVariable 1 3
      y <- newIntegerVariable 1 3
      z <- newIntegerVariable 1 3
      
      allDifferent [x, y, z]

    request = requestByteString program parameters
    actual = B64.encode request
  in 
    actual @?= "Ci4SBBICAQMSBBICAQMSBBICAQMaGmoYCgYKAQASAQEKBgoBARIBAQoGCgECEgEBGhOhAgAAAAAAACRAuAUAoAYBkAwA"


-- helpers
buildBase64ByteString :: (Encode.Encodable a) => a -> BS.ByteString
buildBase64ByteString = B64.encode . Encode.buildByteString

parseFromBase64ByteString :: BS.ByteString -> Either String Response
parseFromBase64ByteString = parseResponse . B64.decodeLenient
