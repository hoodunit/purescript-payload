exports.jsonStringify = function(r) {
  return JSON.stringify(r, null, 2)
}

exports.formatJsonString = function(str) {
  try {
    return JSON.stringify(JSON.parse(str), null, 2)
  } catch (e) {
    return str
  }
}
