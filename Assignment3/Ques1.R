a<-0
b<-10
n<-10
h<-(b-a)/n
h
x<-seq(a,b,h)
x
trap_int <- function(x)
  {
 return(2^x)
}
y <- trap_int(x)
N <- length(y)


sum1 <- 0
for(i in 2:(N-1))
{
  sum1 <- sum1 + 2*y[i]
}
trap_value<-(h/2)*(y[1] + y[N]+sum1)
trap_value

inte<-integrate(trap_int, lower = 0, upper = 10)
