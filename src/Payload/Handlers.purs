module Payload.Handlers where

import Prelude

import Control.Monad.Except (ExceptT(..), lift, throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug.Trace as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (readString)
import Node.FS.Aff as FsAff
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream)
import Payload.Headers (Headers(..))
import Payload.Headers as Headers
import Payload.MimeTypes as MimeTypes
import Payload.Path as Path
import Payload.Response (class EncodeResponse, Response(..), ResponseBody(..), ServerError(..), Handler)
import Payload.Response as Response
import Payload.Status as Status
import Simple.JSON (class ReadForeign)
import Unsafe.Coerce (unsafeCoerce)

data File = File String

instance encodeResponseFile :: EncodeResponse File where
  encodeResponse (Response r@{ body: File path }) = do
    exists <- lift $ FsAff.exists path
    if not exists
      then throwError notFoundError
      else do
        stat <- lift $ FsAff.stat path
        if Stats.isFile stat then do
          fileStream <- lift $ liftEffect $ createReadStream path
          let mimeType = fromMaybe "text/plain" $ MimeTypes.pathToMimeType path
          pure $ Response
             { status: Status.ok
             , headers: Headers.fromFoldable
                 [ Tuple "Content-Type" mimeType
                 , Tuple "Content-Length" (show (fileSize stat))
                 ]
             , body: StreamBody (unsafeCoerce fileStream) }
          else throwError notFoundError

instance readForeignFile :: ReadForeign File where
  readImpl f = File <$> readString f
  
fileSize :: Stats.Stats -> Int
fileSize (Stats.Stats statsObj) = Int.round statsObj.size

file :: forall r. String -> { | r } -> Aff File
file path _ = pure (File path)

directory :: forall f. Foldable f => String -> f String -> Aff (Either ServerError File)
directory root path = do
  rootPath <- pure (Path.resolve [root])
  requestedPath <- pure (Path.resolve $ [root] <> Array.fromFoldable path)
  if String.indexOf (String.Pattern rootPath) requestedPath == Just 0
     then pure $ Right $ File requestedPath
     else pure $ Left notFoundError

notFoundError :: ServerError
notFoundError = Response.serverError Status.notFound "File not found"
