module OrTools.Sat.Model.Request
  ( Request(..)
  , buildRequest
  ) where

import qualified OrTools.Sat.Model.Encode as Encode
import qualified Data.ByteString as BS

import OrTools.Sat.Model.Model
import OrTools.Sat.Model.Parameters


data Request = Request
  { requestModel :: Model
  , requestParameters :: Parameters
  }

instance Encode.Encodable Request where
  build Request{..} = 
    mconcat 
      [ Encode.embedded 1 (Encode.build requestModel)
      , Encode.embedded 3 (Encode.build requestParameters)
      ]


buildRequest :: Request -> BS.ByteString
buildRequest = Encode.buildByteString
