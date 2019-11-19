module Payload.Server.Status where

import Payload.ResponseTypes (HttpStatus)

custom :: Int -> String -> HttpStatus
custom code reason = { code, reason }

continue :: HttpStatus
continue = { code: 100, reason: "Continue" }

switchingProtocols :: HttpStatus
switchingProtocols = { code: 101, reason: "Switching Protocols" }

processing :: HttpStatus
processing = { code: 102, reason: "Processing" }

ok :: HttpStatus
ok = { code: 200, reason: "OK" }

created :: HttpStatus
created = { code: 201, reason: "Created" }

accepted :: HttpStatus
accepted = { code: 202, reason: "Accepted" }

nonAuthoritativeInformation :: HttpStatus
nonAuthoritativeInformation = { code: 203, reason: "Non-Authoritative Information" }

noContent :: HttpStatus
noContent = { code: 204, reason: "No Content" }

resetContent :: HttpStatus
resetContent = { code: 205, reason: "Reset Content" }

partialContent :: HttpStatus
partialContent = { code: 206, reason: "Partial Content" }

multiStatus :: HttpStatus
multiStatus = { code: 207, reason: "Multi-Status" }

alreadyReported :: HttpStatus
alreadyReported = { code: 208, reason: "Already Reported" }

imUsed :: HttpStatus
imUsed = { code: 226, reason: "IM Used" }

multipleChoices :: HttpStatus
multipleChoices = { code: 300, reason: "Multiple Choices" }

movedPermanently :: HttpStatus
movedPermanently = { code: 301, reason: "Moved Permanently" }

found :: HttpStatus
found = { code: 302, reason: "Found" }

seeOther :: HttpStatus
seeOther = { code: 303, reason: "See Other" }

notModified :: HttpStatus
notModified = { code: 304, reason: "Not Modified" }

useProxy :: HttpStatus
useProxy = { code: 305, reason: "Use Proxy" }

temporaryRedirect :: HttpStatus
temporaryRedirect = { code: 307, reason: "Temporary Redirect" }

permanentRedirect :: HttpStatus
permanentRedirect = { code: 308, reason: "Permanent Redirect" }

badRequest :: HttpStatus
badRequest = { code: 400, reason: "Bad Request" }

unauthorized :: HttpStatus
unauthorized = { code: 401, reason: "Unauthorized" }

paymentRequired :: HttpStatus
paymentRequired = { code: 402, reason: "Payment Required" }

forbidden :: HttpStatus
forbidden = { code: 403, reason: "Forbidden" }

notFound :: HttpStatus
notFound = { code: 404, reason: "Not Found" }

methodNotAllowed :: HttpStatus
methodNotAllowed = { code: 405, reason: "Method Not Allowed" }

notAcceptable :: HttpStatus
notAcceptable = { code: 406, reason: "Not Acceptable" }

proxyAuthenticationRequired :: HttpStatus
proxyAuthenticationRequired = { code: 407, reason: "Proxy Authentication Required" }

requestTimeout :: HttpStatus
requestTimeout = { code: 408, reason: "Request Timeout" }

conflict :: HttpStatus
conflict = { code: 409, reason: "Conflict" }

gone :: HttpStatus
gone = { code: 410, reason: "Gone" }

lengthRequired :: HttpStatus
lengthRequired = { code: 411, reason: "Length Required" }

preconditionFailed :: HttpStatus
preconditionFailed = { code: 412, reason: "Precondition Failed" }

payloadTooLarge :: HttpStatus
payloadTooLarge = { code: 413, reason: "Payload Too Large" }

uriTooLong :: HttpStatus
uriTooLong = { code: 414, reason: "URI Too Long" }

unsupportedMediaType :: HttpStatus
unsupportedMediaType = { code: 415, reason: "Unsupported Media Type" }

rangeNotSatisfiable :: HttpStatus
rangeNotSatisfiable = { code: 416, reason: "Range Not Satisfiable" }

expectationFailed :: HttpStatus
expectationFailed = { code: 417, reason: "Expectation Failed" }

imATeapot :: HttpStatus
imATeapot = { code: 418, reason: "I'm a teapot" }

misdirectedRequest :: HttpStatus
misdirectedRequest = { code: 421, reason: "Misdirected Request" }

unprocessableEntity :: HttpStatus
unprocessableEntity = { code: 422, reason: "Unprocessable Entity" }

locked :: HttpStatus
locked = { code: 423, reason: "Locked" }

failedDependency :: HttpStatus
failedDependency = { code: 424, reason: "Failed Dependency" }

upgradeRequired :: HttpStatus
upgradeRequired = { code: 426, reason: "Upgrade Required" }

preconditionRequired :: HttpStatus
preconditionRequired = { code: 428, reason: "Precondition Required" }

tooManyRequests :: HttpStatus
tooManyRequests = { code: 429, reason: "Too Many Requests" }

requestHeaderFieldsTooLarge :: HttpStatus
requestHeaderFieldsTooLarge = { code: 431, reason: "Request Header Fields Too Large" }

unavailableForLegalReasons :: HttpStatus
unavailableForLegalReasons = { code: 451, reason: "Unavailable For Legal Reasons" }

internalError :: HttpStatus
internalError = { code: 500, reason: "Internal Server Error" }

notImplemented :: HttpStatus
notImplemented = { code: 501, reason: "Not Implemented" }

badGateway :: HttpStatus
badGateway = { code: 502, reason: "Bad Gateway" }

serviceUnavailable :: HttpStatus
serviceUnavailable = { code: 503, reason: "Service Unavailable" }

gatewayTimeout :: HttpStatus
gatewayTimeout = { code: 504, reason: "Gateway Timeout" }

httpVersionNotSupported :: HttpStatus
httpVersionNotSupported = { code: 505, reason: "HTTP Version Not Supported" }

variantAlsoNegotiates :: HttpStatus
variantAlsoNegotiates = { code: 506, reason: "Variant Also Negotiates" }

insufficientStorage :: HttpStatus
insufficientStorage = { code: 507, reason: "Insufficient Storage" }

loopDetected :: HttpStatus
loopDetected = { code: 508, reason: "Loop Detected" }

notExtended :: HttpStatus
notExtended = { code: 510, reason: "Not Extended" }

networkAuthenticationRequired :: HttpStatus
networkAuthenticationRequired = { code: 511, reason: "Network Authentication Required" }

