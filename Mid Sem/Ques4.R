stepSize=0.01
x1<-seq(0,(pi/2),stepSize)
x2<-seq((pi/2),pi,stepSize)
x3<-seq(pi,(3*(pi/2)),stepSize)
x4<-seq((3*(pi/2)),(2*pi),stepSize)
x <- c(x1,x2,x3,x4)

fx <- sin(x^2)+(4*cos(x))
fx1 <- sin(x1^2)+(4*cos(x1))
fx2 <- sin(x2^2)+(4*cos(x2))
fx3 <- sin(x3^2)+(4*cos(x3))
fx4 <- sin(x4^2)+(4*cos(x4))
plot(x1, fx1,main="Graphs",ylab="",type="h",xlim = c(0,2*pi),ylim = c(-7,7),col="red")
lines(x2,fx2, col="green",type = "h")
lines(x3,fx3, col="blue",type="h")
lines(x4,fx4, col="gold",type="h")