if(FALSE) {
ctr = genCounter()
trace(match, ctr, print = FALSE)

coll = genCallCollector()
trace(match, coll, print = FALSE)
}

genCounter =
function()
{
  ans = integer()
  function(var = as.character(deparse(sys.call(sys.nframe()-6)[[1]]))){
    ans[var] <<- if(is.na(ans[var])) 1L else ans[var] + 1L
  }
}


genCallCollector =
function()
{
  ans = list()
  function(var = as.character(deparse(sys.call(sys.nframe()-6)[[1]]))){
    i = which(names(ans) == var)
    if(length(i))
      ans[[i]][[length(ans[[i]]) + 1L]] <<- sys.calls()
    else {
      ans[[length(ans) + 1L]] <<- list(sys.calls())
      names(ans)[length(ans)] <<- var
    }
  }
}


getFuns =
function(exprs)
{
  sapply(exprs[seq(1, length = length(exprs) - 5)], getCallFun)
}

getCallFun =
function(e)
{
  if(!is.name(e))
     e = e[[1]]
  as.character(e)
}



genAddTime =
    #
    #  This is used to create a function that is used to collect the
    #  run-time for each call to a function.
    #  Rprof() can do this but doesn't distinguish between different 
    #  functions used in s/l/mapply() calls but considers them all FUN.
    #  So this can be used
    #
    #
    # This is used as
    #   timer = genAddTime(guessNumCalls)
    #
    # Then in the first line of the function
    #    cur = Sys.time(); on.exit(timer$addTime(cur))
    #
function(num)
{
    times = numeric(num)
    ctr = 1
    add = function(start, end = Sys.time()) {
        times[ctr] <<- end - start
        ctr <<- ctr + 1
    }
    list(addTime = add, times = function() times)
}
