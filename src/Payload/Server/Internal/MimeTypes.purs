module Payload.Server.Internal.MimeTypes where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Node.Path as Path

foreign import extensionToMimeTypeImpl :: String -> Nullable String

extensionToMimeType :: String -> Maybe String
extensionToMimeType ext = Nullable.toMaybe (extensionToMimeTypeImpl ext)

safeExtName :: String -> Maybe String
safeExtName path = case removeDots (Path.extname path) of
  "" -> Nothing
  ext -> Just ext

removeDots :: String -> String
removeDots = String.replace (String.Pattern ".") (String.Replacement "")

pathToMimeType :: String -> Maybe String
pathToMimeType = safeExtName >=> extensionToMimeType
