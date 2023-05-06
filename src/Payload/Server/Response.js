import { Readable } from "stream"

export const readableStreamToNodeReadable = (stream) => {
  return Readable.fromWeb(stream)
}
