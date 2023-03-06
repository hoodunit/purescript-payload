export const endResponse_ = res => unit => cb => () => {
  res.end(null, null, function(){
    cb(unit)
  })
}
