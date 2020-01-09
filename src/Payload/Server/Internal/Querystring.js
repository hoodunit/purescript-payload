exports.querystringParse = function(inputStr){
  var qs = require('querystring');
  var parsed = qs.parse(inputStr);
  for (var prop in parsed) {
    if (Object.prototype.hasOwnProperty.call(parsed, prop)) {
      if (typeof parsed[prop] === 'string'){
        parsed[prop] = [parsed[prop]];
      } else if (Array.isArray(parsed[prop])){
        // Do nothing
      } else {
        throw new Error('Expected parsed query string value to be string or array, but received ' + obj[prop]);
      }
    }
  }
  return parsed;
}
