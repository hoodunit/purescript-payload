module Payload.ResponseTypes where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Payload.Headers (Headers)

-- | The type of a server response, before encoding the body.
-- | Responses with modified statuses or headers can be created
-- | by returning this type (directly or via helper functions).
newtype Response r = Response
  { status :: HttpStatus
  , headers :: Headers
  , body :: r }

type ResponseContent r =
  { status :: HttpStatus
  , headers :: Headers
  , body :: r }

type HttpStatus = { code :: Int, reason :: String }

derive instance newtypeResponse :: Newtype (Response a) _
instance eqResponse :: Eq a => Eq (Response a) where
  eq (Response r1) (Response r2) = r1 == r2
instance showResponse :: Show a => Show (Response a) where
  show (Response r) = show r

-- | An empty response body
data Empty = Empty

-- | A JSON response body
newtype Json a = Json a

-- | All server error responses ultimately resolve into this type
data Failure = Forward String | Error RawResponse
instance showFailure :: Show Failure where
  show (Forward s) = "Forward '" <> s <> "'"
  show (Error e) = "Error " <> show e

-- | All server responses ultimately resolve into this type. 
type RawResponse = Response ResponseBody

-- | The base types of body responses that can be returned.
data ResponseBody = StringBody String | StreamBody UnsafeStream | EmptyBody
data UnsafeStream

instance eqResponseBody :: Eq ResponseBody where
  eq (StringBody s1) (StringBody s2) = s1 == s2
  eq EmptyBody EmptyBody = true
  eq (StreamBody _) (StreamBody _) = false
  eq _ _ = false
instance showResponseBody :: Show ResponseBody where
  show (StringBody s) = s
  show EmptyBody = "EmptyBody"
  show (StreamBody _) = "StreamBody"

-- | Internally handlers and guards all de-sugar into this type.
type Result m = ExceptT Failure m

