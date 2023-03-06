export const encodeUri = (str) => {
  try {
    return encodeURIComponent(str)
  } catch (e) {
    console.error(e)
    return str
  }
}
