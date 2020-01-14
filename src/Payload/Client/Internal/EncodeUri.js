exports.encodeUri = function(str) {
  try {
    return encodeURIComponent(str)
  } catch (e) {
    console.log(e)
    return str
  }
}
