png("r2d.png", 800, 800, bg = "transparent")
plot(ans$x, ans$y, xlab = "", ylab = "", axes = FALSE)
lines(ans$x, ans$y)
dev.off()
