x <- seq(0,8,1)
y1 <- 2*x
y2 <- 2.5*x
plot(x,y1,type = "l",xlim=c(-1,3),ylim=c(-1,3),col="green")
lines(y2,col="red")
abline(h=0)
abline(v=0)
legend("topleft", legend=c("y1 = 2*x", "y2 = 2.5*x"),col=c("green", "red"), lty=1:1, cex=0.8)

