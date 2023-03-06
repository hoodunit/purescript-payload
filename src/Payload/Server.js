export const onError = server => cb => () => {
  server.on("error", (error) => {
    cb(error)()
  })
}
