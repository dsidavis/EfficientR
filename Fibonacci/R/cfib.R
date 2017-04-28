cfib =
function(n)
{
  .C("R_fib", as.integer(n), ans = 0L)$ans
}

