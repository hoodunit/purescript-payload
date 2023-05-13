-- | Contains various built-in handlers for e.g. returning static files.
module Payload.Server.Handlers
       ( directory
       , file
       ) where

import Prelude

import Control.Monad.Except (lift, throwError)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Foreign (readString)
import Node.FS.Aff as FsAff
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream)
import Node.Stream (Read, Stream)
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..), Response(..), ResponseBody(..), Result, RawResponse)
import Payload.Server.Internal.MimeTypes as MimeTypes
import Payload.Server.Path as Path
import Payload.Server.Response (class EncodeResponse)
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Simple.JSON (class ReadForeign)
import Unsafe.Coerce (unsafeCoerce)

-- | Handler for returning a file at the given path.
-- | Attempts to return an appropriate MIME type, defaulting
-- | to "text/plain".
-- | Fails with 404 Not Found if file cannot be found.
file :: String -> Aff (Either Failure (Response (Stream (read :: Read))))
file path = do
  statResult <- Aff.attempt (FsAff.stat path)
  case statResult of
    Right stat | Stats.isFile stat -> do
      fileStream <- liftEffect $ createReadStream path
      let mimeType = fromMaybe "text/plain" $ MimeTypes.pathToMimeType path
      pure $ Right $ Response
        { status: Status.ok
        , headers: Headers.fromFoldable
        [ Tuple "Content-Type" mimeType
        , Tuple "Content-Length" (show (fileSize stat))
        ]
        , body: fileStream }
    _ -> pure (Left notFoundError)

fileSize :: Stats.Stats -> Int
fileSize (Stats.Stats statsObj) = Int.round statsObj.size

-- | Handler for returning static files from a directory.
-- | Protects against directory traversal attacks.
directory :: forall f
             . Foldable f
             => String
             -> f String
             -> Aff (Either Failure (Response (Stream (read :: Read))))
directory root path = do
  rootPath <- pure (Path.resolve [root])
  requestedPath <- pure (Path.resolve $ [root] <> Array.fromFoldable path)
  if String.indexOf (String.Pattern rootPath) requestedPath == Just 0
     then file requestedPath
     else pure $ Left notFoundError

notFoundError :: Failure
notFoundError = Error (Response.notFound (StringBody "File not found"))
