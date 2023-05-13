import {Readable} from "stream"
import {ReadableStream} from 'node:stream/web'

export const streamToStringImpl = (stream) => () => {
  const chunks = []
  const reader = stream.getReader()
  const readChunk = () => {
    return reader.read().then(({ done, value }) => {
      const decoded = new TextDecoder().decode(value, {stream: true})
      chunks.push(decoded)
      if (done) {
        return chunks.join('')
      }
      return readChunk()
    })
  }
  return readChunk().catch((error) => {
    reader.releaseLock()
    throw error
  })
}

export const stringsToStream = (strings) => {
  const stream = new ReadableStream({
    async start(controller) {
      for(const str of strings) {
        controller.enqueue(str)
      }
      setTimeout(() => {
        controller.close()
      }, 0);
    }
  })
  return stream
}

export const stringsToNodeStream = (strings) => {
  const stream = new Readable()
  stream._read = () => {}
  for (const string of strings) {
    stream.push(string)
  }
  stream.push(null)
  return stream
}
