{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Response where

import           Data.Aeson hiding (Result)
import           Data.Text (pack)
import           Data.ByteString hiding (pack)
import           GHC.Generics

data Headers = Headers [(String, String)]
  deriving (Generic)
instance ToJSON Headers where
  toJSON (Headers list) = object [ (pack fn) .= toJSON val | (fn, val) <- list]

data APIGatewayResponse = APIGatewayResponse
  { statusCode :: Int
  , headers :: Headers 
  , body :: String
  } deriving (Generic)
instance ToJSON APIGatewayResponse 

getHeaders :: APIGatewayResponse -> [(String, String)]
getHeaders (APIGatewayResponse {headers = (Headers x)}) = x

data Result = Valid
            | CounterExample
                { initialSlot :: Integer
                , transactionList :: String
                , transactionWarning :: String
                }
  deriving (Generic)
instance FromJSON Result 
instance ToJSON Result

data Response = Response
  { uuid :: String
  , result :: Result 
  } deriving (Generic)
instance FromJSON Response 
instance ToJSON Response

