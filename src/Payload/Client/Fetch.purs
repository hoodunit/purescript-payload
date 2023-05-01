module Payload.Client.Fetch where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Method (Method)
import Payload.ResponseTypes (HttpStatus)
import Web.Streams.ReadableStream (ReadableStream)

type FetchOptions =
  { method :: Method
  , body :: Maybe String
  , headers :: Headers }

type RawFetchOptions =
  { method :: String
  , body :: Nullable String
  , headers :: Object String }

data RawFetchResponse

type FetchResponse =
  { status :: HttpStatus
  , headers :: Headers
  , raw :: RawFetchResponse }

data FetchHeaders

foreign import fetchImpl :: String -> RawFetchOptions -> Effect (Promise RawFetchResponse)
foreign import responseHeaders :: RawFetchResponse
                                  -> (String -> String -> Tuple String String)
                                  -> Array (Tuple String String)
foreign import responseStatus :: RawFetchResponse -> HttpStatus
foreign import responseText :: RawFetchResponse -> Effect (Promise String)
foreign import responseBody :: (ReadableStream Uint8Array -> Maybe (ReadableStream Uint8Array))
                               -> Maybe (ReadableStream Uint8Array)
                               -> RawFetchResponse
                               -> Maybe (ReadableStream Uint8Array)

toRawOptions :: FetchOptions -> RawFetchOptions
toRawOptions opts =
  { method: show opts.method
  , body: Nullable.toNullable opts.body
  , headers: Object.fromFoldable (Headers.toUnfoldable opts.headers :: Array (Tuple String String)) }

fetch :: String -> FetchOptions -> Aff (Either Error FetchResponse)
fetch url opts = Aff.try $ do
  resp <- Promise.toAffE (fetchImpl url (toRawOptions opts))
  pure { status: responseStatus resp
       , headers: Headers.fromFoldable (responseHeaders resp Tuple)
       , raw: resp }

text :: RawFetchResponse -> Aff (Either Error String)
text resp = Aff.try $ Promise.toAffE (responseText resp)

body :: RawFetchResponse -> Maybe (ReadableStream Uint8Array)
body resp = responseBody Just Nothing resp
