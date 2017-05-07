N = 100
size = rpois(N, 10) + 1L
p = runif(length(N))

x1 = rbinom(N, size, .5)
x2 = rbinom(N, size, p)

y1  = sapply(size, rbinom, .5)
y2  = mapply(rbinom, size, p)

