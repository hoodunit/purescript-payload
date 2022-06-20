export function unsafeParseIso8601DateImpl(dateStr) {
  return new Date(dateStr)
} 

export function readEnvVarImpl(nothing) {
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
