isAssignReturn =
function(fun)
{
  if(is.character(fun))
     fun = get(fun)

  b = body(fun)
  n = length(b)
  if(b[[n]][[1]] == "return") 
    return(is.symbol(b[[n]][[2]]) && as.character(b[[n-1]][[1]]) %in% c('=', '<-') &&    b[[n]][[2]] == b[[n-1]][[2]])

 FALSE
}

# sapply(simFuns, isAssignReturn)
