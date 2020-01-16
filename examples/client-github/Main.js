exports.unsafeParseIso8601DateImpl = function(dateStr) {
  return new Date(dateStr)
} 

exports.readEnvVarImpl = function(nothing) {
  return function(just) {
    return function(key) {
      return function() {
        var val = require('process').env[key]
        if (val) {
          return just(val)
        } else {
          return nothing
        }
      }
    }
  }
}
