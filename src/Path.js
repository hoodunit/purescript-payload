exports.resolve = function(paths){
  return require('path').resolve.apply(this, paths);
}
