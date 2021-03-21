module Payload.Client.Response where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff, throwError)
import Effect.Exception as Ex
import Payload.Client.DecodeResponse (DecodeResponseError)
import Payload.ResponseTypes (Response, ResponseContent)

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

unwrapResponse :: forall a. Aff (ClientResponse a) -> Aff (ResponseContent a)
unwrapResponse aff = do
  result <- aff
  case result of
    Right response -> pure (unwrap response)
    Left err -> throwError (unwrapError err)
  where
    unwrapError :: ClientError -> Ex.Error
    unwrapError err = Ex.error $ "Error unwrapping response from Payload ClientResponse - attempted to unwrap " <>
      "client response but response contained error: \n" <> show err

unwrapBody :: forall a. Aff (ClientResponse a) -> Aff a
unwrapBody aff = do
  result <- aff
  case result of
    Right response -> pure $ (unwrap response).body
    Left err -> throwError (unwrapError err)
  where
    unwrapError :: ClientError -> Ex.Error
    unwrapError err = Ex.error $ "Error unwrapping response body from Payload ClientResponse- attempted to unwrap " <>
      "client response body but response contained error: \n" <> show err
