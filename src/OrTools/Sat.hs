module OrTools.Sat 
  ( SatSolver
  , Sat(..)
  , SatModel(..)
  , requestByteString
  , solve

  -- re-exports
  , Parameters(..)
  , defaultParameters
  , variableNot
  ) where

import Data.Int (Int64)
import Control.Monad.State
import Data.ByteString (ByteString)

import OrTools.Sat.Model.Model
import OrTools.Sat.Model.Constant
import OrTools.Sat.Model.Variable
import OrTools.Sat.Model.Constraint
import OrTools.Sat.Model.LinearExpression
import OrTools.Sat.Model.Objective
import OrTools.Sat.Model.Request
import OrTools.Sat.Model.Response
import OrTools.Sat.Model.Parameters
import OrTools.Sat.Model.Encode


type SatSolver m = Request -> m Response


class Monad m => Sat m where
  -- variables + constants
  newIntegerVariable :: Int64 -> Int64 -> m Variable
  newBooleanVariable :: m Variable
  newConstant :: (Integral a) => a -> m Constant

  -- constraints
  allDifferent :: (ToLinearExpression a) => [a] -> m ()
  equality :: (ToLinearExpression a, ToLinearExpression b) => a -> b -> m ()
  equalityIf :: (ToLinearExpression a, ToLinearExpression b) => a -> b -> Variable -> m ()
  notEqual :: (ToLinearExpression a, ToLinearExpression b) => a -> b -> m ()
  notEqualIf :: (ToLinearExpression a, ToLinearExpression b) => a -> b -> Variable -> m ()
  minEquality :: (ToLinearExpression a, ToLinearExpression b) => a -> [b] -> m ()
  maxEquality :: (ToLinearExpression a, ToLinearExpression b) => a -> [b] -> m ()
  lessOrEqual :: (ToLinearExpression a, ToLinearExpression b) => a -> b -> m ()

  -- objectives
  minimise :: (ToLinearExpression a) => a -> m ()

  -- operations
  notBool :: Variable -> m Variable


newtype SatModel a = SatModel { unSatModel :: State Model a }
  deriving (Functor, Applicative, Monad)


instance Sat SatModel where
  -- variables + constants
  newIntegerVariable lowerBound upperBound = do
    model <- SatModel get
    
    let reference = length (modelVariables model)
        variable = IntegerVariable (fromIntegral reference) lowerBound upperBound

    SatModel (modify $ \m -> m { modelVariables = modelVariables m <> [variable] })

    return variable

  newBooleanVariable = do
    model <- SatModel get
    
    let reference = length (modelVariables model)
        variable = BooleanVariable (fromIntegral reference) False

    SatModel (modify $ \m -> m { modelVariables = modelVariables m <> [variable] })
    
    return variable


  newConstant value = do
    return (Constant (fromIntegral value))

  -- constraints
  allDifferent range = do
    let rangeExpressions = toLinearExpression <$> range
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [AllDifferent rangeExpressions] })


  equality left right = do
    let leftExpression = toLinearExpression left
        rightExpression = toLinearExpression right
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [Equality leftExpression rightExpression] })


  equalityIf left right predicate = do
    let leftExpression = toLinearExpression left
        rightExpression = toLinearExpression right
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [Enforcement predicate (Equality leftExpression rightExpression)] })


  notEqual left right = do
    let leftExpression = toLinearExpression left
        rightExpression = toLinearExpression right
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [NotEqual leftExpression rightExpression] })

  notEqualIf left right predicate = do
    let leftExpression = toLinearExpression left
        rightExpression = toLinearExpression right
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [Enforcement predicate (NotEqual leftExpression rightExpression)] })


  minEquality target range = do
    let targetExpression = toLinearExpression target
        rangeExpressions = toLinearExpression <$> range
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [MinEquality targetExpression rangeExpressions] })


  maxEquality target range = do
    let targetExpression = toLinearExpression target
        rangeExpressions = toLinearExpression <$> range
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [MaxEquality targetExpression rangeExpressions] })


  lessOrEqual left right = do
    let leftExpression = toLinearExpression left
        rightExpression = toLinearExpression right
    
    SatModel (modify $ \m -> m { modelConstraints = modelConstraints m <> [LessOrEqual leftExpression rightExpression] })

  -- objectives
  minimise target = do
    let targetExpression = toLinearExpression target

    SatModel (modify $ \m -> m { modelObjective = Just (minimiseObjective targetExpression) })

  -- operations
  notBool variable = do
    return (variableNot variable)


requestByteString :: SatModel m -> Parameters -> ByteString
requestByteString model parameters = 
  let
    model' = snd $ runState (unSatModel model) emptyModel
    request = Request { requestModel = model', requestParameters = parameters }
  in 
    buildByteString request


solve :: SatSolver IO -> SatModel m -> Parameters -> IO Response
solve solver model parameters =
  let
    model' = snd $ runState (unSatModel model) emptyModel
    request = Request { requestModel = model', requestParameters = parameters }
  in do
    solver request
