module OrTools.Sat.Model.Encode
  ( Encodable(..)
  , optional
  , nonZeroDouble
  , nonZeroInt64
  , buildByteString

  -- re-exports
  , Encode.MessageBuilder
  , Encode.embedded
  , Encode.packedVarints
  , Encode.bool
  , Encode.double
  , Encode.int32
  , Encode.int64
  ) where

import Data.Int (Int64)
import Proto3.Wire.Types (FieldNumber)

import qualified Proto3.Wire.Encode as Encode
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS


class Encodable a where
  build :: a -> Encode.MessageBuilder


optional :: (a -> Encode.MessageBuilder) -> Maybe a -> Encode.MessageBuilder
optional = maybe mempty 


nonZeroDouble :: FieldNumber -> Double -> Encode.MessageBuilder
nonZeroDouble _ 0 = mempty
nonZeroDouble fieldNumber value = Encode.double fieldNumber value


nonZeroInt64 :: FieldNumber -> Int64 -> Encode.MessageBuilder
nonZeroInt64 _ 0 = mempty
nonZeroInt64 fieldNumber value = Encode.int64 fieldNumber value


buildByteString :: (Encodable a) => a -> BS.ByteString
buildByteString = BL.toStrict . Encode.toLazyByteString . build