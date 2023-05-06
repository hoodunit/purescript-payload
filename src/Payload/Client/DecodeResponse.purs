module Payload.Client.DecodeResponse where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import Node.Stream as Stream
import Payload.Client.Fetch (FetchResponse)
import Payload.Client.Fetch as Fetch
import Payload.ResponseTypes (ResponseBody(..))
import Payload.Server.Handlers (File(..))
import Payload.TypeErrors (type (<>), type (|>))
import Prim.TypeError (class Warn, Quote, Text)
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)
import Web.Streams.ReadableStream (ReadableStream)

data DecodeResponseError
  = InternalDecodeError { message :: String }
  | UnhandledResponseError { message :: String }
  | JsonDecodeError { body :: String, errors :: MultipleErrors }
  | UnknownError { message :: String }

instance showDecodeResponseError :: Show DecodeResponseError where
  show (InternalDecodeError { message }) = "InternalDecodeError '" <> message <> "'"
  show (UnhandledResponseError { message }) = "UnhandledResponseError '" <> message <> "'"
  show (JsonDecodeError { errors, body }) = "JsonDecodeError: " <> show errors <> "'\nBody: " <> body
  show (UnknownError { message }) = "UnknownError: '" <> message <> "'"
instance eqDecodeResponseError :: Eq DecodeResponseError where
  eq (InternalDecodeError a) (InternalDecodeError b) = a == b
  eq (UnhandledResponseError a) (UnhandledResponseError b) = a == b
  eq (JsonDecodeError a) (JsonDecodeError b) = a == b
  eq (UnknownError a) (UnknownError b) = a == b
  eq _ _ = false

unexpectedError :: forall a. String -> ResponseBody -> Either DecodeResponseError a
unexpectedError expected body = Left (InternalDecodeError { message })
  where
    received = case body of
      StringBody s -> "(StringBody '" <> s <> "')"
      StreamBody _ -> "StreamBody"
      EmptyBody -> "EmptyBody"
    message = "Invalid response type, expected '" <> expected <> "' but received '" <> received <> "'." <>
      "This is probably a bug in the library."

unhandled :: String -> DecodeResponseError
unhandled message = UnhandledResponseError { message }

unknown :: String -> DecodeResponseError
unknown message = UnknownError { message }

jsonDecodeError :: String -> MultipleErrors -> DecodeResponseError
jsonDecodeError body errors = JsonDecodeError { body, errors }

class DecodeResponse body where
  decodeResponse :: FetchResponse -> Aff (Either DecodeResponseError body)

instance decodeResponseString :: DecodeResponse String where
  decodeResponse resp = Fetch.text resp.raw
                        # map (lmap (show >>> unknown))
else instance decodeResponseReadableStream :: DecodeResponse (ReadableStream Uint8Array) where
  decodeResponse resp = case Fetch.body resp.raw of 
    Just body -> pure (Right body)
    Nothing -> pure (Left (unknown "Stream body was empty"))
else instance decodeResponseRecord ::
  ( SimpleJson.ReadForeign (Record r)
  ) => DecodeResponse (Record r) where
  decodeResponse resp = do
    text <- Fetch.text resp.raw
    pure $ text # lmap (show >>> unknown)
           >>= (\body -> SimpleJson.readJSON body # lmap (jsonDecodeError body))
else instance decodeResponseArray ::
  ( SimpleJson.ReadForeign (Array r)
  ) => DecodeResponse (Array r) where
  decodeResponse resp = do
    text <- Fetch.text resp.raw
    pure $ text # lmap (show >>> unknown)
           >>= (\body -> SimpleJson.readJSON body # lmap (jsonDecodeError body))
else instance decodeResponseFile :: DecodeResponse File where
  decodeResponse resp = decodeResponseUnimplemented resp

decodeResponseUnimplemented :: forall body
  . Warn (Text "API client cannot query all of endpoints in API spec:"
  |> Text ""
  |> Text "No type class instance was found for"
  |> Text ""
  |> Text "DecodeResponse " <> Quote body
  |> Text "")
  => DecodeResponse body
  => FetchResponse -> Aff (Either DecodeResponseError body)
decodeResponseUnimplemented _ = pure (Left (unhandled "Could not decode response - no DecodeResponse instance"))
