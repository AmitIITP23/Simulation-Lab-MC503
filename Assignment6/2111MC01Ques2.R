x <- seq(-pi,pi,0.05)
y <- 1/x
plot(x,y,xlab="x",ylab = "f(x) = 1/x",main = "Function: y = 1/x",col="green",
     ylim = c(-pi,pi))
lines(x,x,col='blue')
lines(x,-x,col='blue')
abline(h=0,col='red')
abline(v=0,col= 'red')
