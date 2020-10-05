module Payload.ContentType where

import Data.Maybe (Maybe)
import Effect.Exception.Unsafe (unsafeThrow)
import Payload.Internal.Route (Undefined)
import Payload.ResponseTypes (Json)
import Type.Proxy (Proxy(Proxy))

class HasContentType :: forall k. k -> Constraint
class HasContentType a where
  getContentType :: Proxy a -> String

instance hasContentTypeUndefined :: HasContentType Undefined where
  getContentType _ = unsafeThrow "Attempted to get a content type for type Undefined. This is probably a bug in the library."

instance hasContentTypeString :: HasContentType String where
  getContentType _ = plain

instance hasContentTypeJson :: HasContentType (Json a) where
  getContentType _ = json

instance hasContentTypeRecord :: HasContentType (Record r) where
  getContentType _ = json

instance hasContentTypeArray :: HasContentType (Array r) where
  getContentType _ = json

instance hasContentTypeMaybe :: HasContentType a => HasContentType (Maybe a) where
  getContentType _ = getContentType (Proxy :: _ a)

type ContentType = String

any :: ContentType
any = "*/*"

binary :: ContentType
binary = "application/octet-stream"

html :: ContentType
html = "text/html; charset=utf-8"

plain :: ContentType
plain = "text/plain; charset=utf-8"

json :: ContentType
json = "application/json"

msgPack :: ContentType
msgPack = "application/msgpack"

form :: ContentType
form = "application/x-www-form-urlencoded"

javaScript :: ContentType
javaScript = "application/javascript"

css :: ContentType
css = "text/css; charset=utf-8"

formData :: ContentType
formData = "multipart/form-data"

xml :: ContentType
xml = "text/xml; charset=utf-8"

csv :: ContentType
csv = "text/csv; charset=utf-8"

png :: ContentType
png = "image/png"

gif :: ContentType
gif = "image/gif"

bmp :: ContentType
bmp = "image/bmp"

jpeg :: ContentType
jpeg = "image/jpeg"

webp :: ContentType
webp = "image/webp"

svg :: ContentType
svg = "image/svg+xml"

icon :: ContentType
icon = "image/x-icon"

webm :: ContentType
webm = "video/webm"

webmAudio :: ContentType
webmAudio = "audio/webm"

ogg :: ContentType
ogg = "video/ogg"

flac :: ContentType
flac = "audio/flac"

wav :: ContentType
wav = "audio/wav"

pdf :: ContentType
pdf = "application/pdf"

ttf :: ContentType
ttf = "application/font-sfnt"

otf :: ContentType
otf = "application/font-sfnt"

woff :: ContentType
woff = "application/font-woff"

woff2 :: ContentType
woff2 = "font/woff2"

jsonApi :: ContentType
jsonApi = "application/vnd.api+json"

wasm :: ContentType
wasm = "application/wasm"

tiff :: ContentType
tiff = "image/tiff"

aac :: ContentType
aac = "audio/aac"

calendar :: ContentType
calendar = "text/calendar"

mpeg :: ContentType
mpeg = "video/mpeg"

tar :: ContentType
tar = "application/x-tar"

gzip :: ContentType
gzip = "application/gzip"

mov :: ContentType
mov = "video/quicktime"

mp4 :: ContentType
mp4 = "video/mp4"

zip :: ContentType
zip = "application/zip"
