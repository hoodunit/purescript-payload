export function onError(server) {
  return function(cb){
    return function(){
      server.on("error", function(error){
        cb(error)()
      })
    }
  }
}
