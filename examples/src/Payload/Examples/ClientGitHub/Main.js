import * as process from "process"

export const unsafeParseIso8601DateImpl = (dateStr) => {
  return new Date(dateStr)
} 

export const readEnvVarImpl = nothing => just => key => () => {
  var val = process.env[key]
  if (val) {
    return just(val)
  } else {
    return nothing
  }
}
