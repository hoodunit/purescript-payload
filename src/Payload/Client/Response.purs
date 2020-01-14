module Payload.Client.Response where

import Prelude

import Data.Either (Either)
import Payload.Client.DecodeResponse (DecodeResponseError)
import Payload.ResponseTypes (Response(..))

type ClientResponse body = Either ClientError (Response body)
data ClientError
  = DecodeError { error :: DecodeResponseError, response :: Response String }
  | StatusError { response :: Response String }
  | RequestError { message :: String }

instance showClientError :: Show ClientError where
  show (DecodeError err) = "DecodeError: " <> show err
  show (StatusError err) = "StatusError: " <> show err
  show (RequestError err) = "RequestError: " <> show err
instance eqClientError :: Eq ClientError where
  eq (DecodeError a) (DecodeError b) = a == b
  eq (StatusError a) (StatusError b) = a == b
  eq (RequestError a) (RequestError b) = a == b
  eq _ _ = false
