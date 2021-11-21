samples = 2000
x <- numeric(0)
genSample <- function(alpha,beta){
  for(i in 1:samples){
    t <- runif(1)^(1/alpha)
    x[i] <- (t/(1-t))^(1/beta)
  }
  return(x)
}
sampleB <- genSample(1,1.5)
summary(sampleB)
var(sampleB)

xpdf <- genSample(1,1)
ypdf <- 1*1*xpdf**(-1-1)*(1+xpdf^(-1))^(-1-1) 

xcdf <- genSample(1.5,3)
ycdf <- (1+xcdf^(-3))^-1.5

plot(xpdf,ypdf,xlim = c(0.01,5),col="red",xlab = "x",ylab = "y")
points(xcdf,ycdf,col="black")

axis(1, at = seq(0.01, 5, by = 0.01))

legend("topright",legend = c("pdf","cdf"),col = c("red","black"),cex = 0.9,pch = 1)
