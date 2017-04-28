
N = 1e5


system.time({
ans = integer()
for(i in 1:N)
   ans[i] = i
})

system.time({
ans = integer(N)
for(i in 1:N)
   ans[i] = i
})


Ns = 10^(1:6)
slow = sapply(Ns, function(N) system.time({
                                ans = integer()
                                for(i in 1:N)
                                    ans[i] = i
                               }))

fast = sapply(Ns, function(N) system.time({
                                ans = integer(N)
                                for(i in 1:N)
                                    ans[i] = i
                               }) )   

colnames(slow) = colnames(fast) = Ns

png("concatSlowdown.png", 1000, 1000)
matplot(Ns[1:6], cbind(slow[3,], fast[3, 1:6]), type = "l", xlab = "Vector Length N", ylab = "seconds")
points(Ns[1:6], slow[3,])
points(Ns[1:6], fast[3,1:6], col = "red")
title("Concatenation Slow Down")
dev.off()
