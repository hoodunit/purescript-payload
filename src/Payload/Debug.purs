module Payload.Debug where

import Prelude

import Data.Either (Either(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Payload.Headers (Headers, toUnfoldable)
import Payload.ResponseTypes (Response(..))

foreign import jsonStringify :: forall a. a -> String
foreign import formatJsonString :: String -> String

class ShowDebug a where
  showDebug :: a -> String

instance showDebugResponse :: ShowDebug a => ShowDebug (Response a) where
  showDebug (Response r) =
    "Status: " <> show r.status.code <> " " <> r.status.reason <> "\n" <>
    "Headers:\n" <> showHeaders r.headers <> "\n" <>
    "Body:\n" <> showDebug r.body
    where
      showHeaders :: Headers -> String
      showHeaders headers = "  " <> String.joinWith "\n  " (showHeader <$> toUnfoldable headers)
      showHeader :: Tuple String String -> String
      showHeader (Tuple key val) = show key <> " " <> show val
else instance showDebugEither :: (ShowDebug a, ShowDebug b) => ShowDebug (Either a b) where
  showDebug (Right a) = "(Right " <> showDebug a <> ")"
  showDebug (Left a) = "(Left " <> showDebug a <> ")"
else instance showDebugArray :: ShowDebug (Array a) where
  showDebug r = jsonStringify r
else instance showDebugRecord :: ShowDebug (Record a) where
  showDebug r = jsonStringify r
else instance showDebugDefault :: Show a => ShowDebug a where
  showDebug = show
