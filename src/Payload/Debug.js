export const jsonStringify = (r) => {
  return JSON.stringify(r, null, 2)
}

export const formatJsonString = (str) => {
  try {
    return JSON.stringify(JSON.parse(str), null, 2)
  } catch (e) {
    return str
  }
}
