
whatCalls = 
function(fun, funNames = names(calls), calls = lapply(funNames, findGlobals), recursive = TRUE)
{
  k = intersect(calls[[fun]], funNames)
  if(recursive) {
     c(fun, lapply(k, whatCalls, funNames, calls, recursive = TRUE))
  } else
     k
}