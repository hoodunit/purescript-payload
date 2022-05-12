export function endResponse_(res) {
  return function(unit){
    return function(cb){
      return function(){
        res.end(null, null, function(){
          cb(unit)
        })
      }
    }
  }
}
