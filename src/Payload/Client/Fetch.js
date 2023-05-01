export const fetchImpl = (url) => (opts) => () => {
  const {body, headers, ...rest} = opts
  const bodyObj = (body ? {body} : {})
  const headersObj = Object.entries(headers).length > 0 ? {headers} : {}
  const fullOpts = {...rest, ...bodyObj, ...headersObj}
  return fetch(url, fullOpts)
}

export const responseHeaders = (resp) => (tuple) => {
  const newHeaders = []
  resp.headers.forEach((value, key) => {
    newHeaders.push(tuple(key)(value))
  })
  return newHeaders
}

export const responseStatus = (resp) => {
  return {
    code: resp.status,
    reason: resp.statusText
  }
}

export const responseText = (resp) => () => {
  return resp.text()
}

export const responseBody = (just) => (nothing) => (resp) => {
  if (resp.body) {
    return just(resp.body)
  } else {
    return nothing
  }
}
