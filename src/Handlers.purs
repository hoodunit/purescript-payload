module Payload.Handlers where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (readString)
import Node.FS.Aff as FsAff
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream)
import Payload.MimeTypes as MimeTypes
import Payload.Response (class Responder, RawResponse(..), ResponseBody(..))
import Payload.Status as Status
import Simple.JSON (class ReadForeign)
import Unsafe.Coerce (unsafeCoerce)

data File = File String

instance responderFile :: Responder File where
  mkResponse (File path) = do
    stat <- FsAff.stat path
    let mimeType = fromMaybe "text/plain" $ MimeTypes.pathToMimeType path
    if Stats.isFile stat
       then do
         fileStream <- liftEffect $ createReadStream path
         pure $ Right $ RawResponse
           { status: Status.ok
           , headers: Map.fromFoldable
               [ Tuple "Content-Type" mimeType
               , Tuple "Content-Length" (show (fileSize stat))
               ]
           , body: StreamBody (unsafeCoerce fileStream) }
       else pure (Left "Could not read file")

instance readForeignFile :: ReadForeign File where
  readImpl f = File <$> readString f
  
fileSize :: Stats.Stats -> Int
fileSize (Stats.Stats statsObj) = Int.round statsObj.size

type FileHandler = forall r. { | r } -> Aff File

file :: String -> FileHandler
file path _ = pure (File path)
