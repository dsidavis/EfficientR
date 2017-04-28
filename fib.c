
int 
fib(int n)
{
    if(n < 2)
	return(n);
    else
	return(fib(n - 1) + fib(n - 2));
}

void
R_fib(int *rn, int *ans)
{
    *ans = fib( *rn );
}
